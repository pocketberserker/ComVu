module ComVu.Analyzer

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let private checker = FSharpChecker.Create(keepAssemblyContents = true)

let private (|BuilderInstance|_|) instance expr =
  match expr with
  | Some(BasicPatterns.Value memberOrFunc) when memberOrFunc = instance -> Some ()
  | _ -> None

let private displayArgs (args: FSharpMemberOrFunctionOrValue) =
  if args.DisplayName = "unitVar" then "()"
  else args.DisplayName

let private optimizeBindLambdaArg = function
| Lambda(_, Let(name, _, body)) -> Success(Lambda(name, body))
| Lambda(_, body) -> Success(Lambda("()", body))
| expr -> Failure ["failed to optimize Bind lambda term."; sprintf "%O" expr ]

let rec private analyzeBody instance = function
| BasicPatterns.Const(v, t) ->
  match v with
  | null ->
    let name = t.TypeDefinition.CompiledName
    if name = "unit" then "()"
    else "null"
  | _ -> v.ToString()
  |> Const
  |> Success
| BasicPatterns.Value(memberOrFunctionOrValue) -> Success(Value memberOrFunctionOrValue.CompiledName)
| BasicPatterns.NewObject(_, _, args) ->
  args
  |> List.map (analyzeBody instance)
  |> AnalysisResult.sequence
  |> AnalysisResult.map NewObject
| BasicPatterns.NewArray(_, values) ->
  values
  |> List.map (analyzeBody instance)
  |> AnalysisResult.sequence
  |> AnalysisResult.map NewArray
| BasicPatterns.NewTuple(_, values) ->
  values
  |> List.map (analyzeBody instance)
  |> AnalysisResult.sequence
  |> AnalysisResult.map NewTuple
| BasicPatterns.NewUnionCase(_, case, fields) ->
  fields
  |> List.map (analyzeBody instance)
  |> AnalysisResult.sequence
  |> AnalysisResult.map (fun fields -> NewUnionCase(case.Name, fields))
| BasicPatterns.Coerce(_, expr) -> analyzeBody instance expr
| BasicPatterns.Lambda(args, expr) ->
  analyzeBody instance expr
  |> AnalysisResult.map (fun body -> Lambda(displayArgs args, body))
| BasicPatterns.Let((name, value), body) ->
  result {
    let! value = analyzeBody instance value
    let! body = analyzeBody instance body
    return Let(name.CompiledName, value, body)
  }
| BasicPatterns.Call(BuilderInstance instance, member', _, _, exprs) ->
  match (member'.CompiledName, exprs) with
  | ("Zero", [_]) -> Success(Zero instance.CompiledName)
  | ("Return", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> Return(instance.CompiledName, value))
  | ("Yield", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> Yield(instance.CompiledName, value))
  | ("ReturnFrom", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> ReturnBang(instance.CompiledName, value))
  | ("YieldFrom", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> YieldBang(instance.CompiledName, value))
  | ("Bind", [src; lambda]) ->
    result {
      let! src = analyzeBody instance src
      let! lambda = analyzeBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return LetBang(instance.CompiledName, src, lambda)
    }
  | ("Using", [src; lambda]) ->
    result {
      let! src = analyzeBody instance src
      let! lambda = analyzeBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return Use(instance.CompiledName, src, lambda)
    }
  | ("While", [cond; body]) ->
    result {
      let! cond = analyzeBody instance cond
      let! body = analyzeBody instance body
      return While(instance.CompiledName, cond, body)
    }
  | ("For", [src; lambda]) ->
    result {
      let! src = analyzeBody instance src
      let! lambda = analyzeBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return For(instance.CompiledName, src, lambda)
    }
  | ("TryWith", [src; lambda]) ->
    result {
      let! src = analyzeBody instance src
      let! lambda = analyzeBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return TryWith(instance.CompiledName, src, lambda)
    }
  | ("TryFinally", [src; lambda]) ->
    result {
      let! src = analyzeBody instance src
      let! lambda = analyzeBody instance lambda
      return TryFinally(instance.CompiledName, src, lambda)
    }
  | ("Combine", [expr1; expr2]) ->
    result {
      let! expr1 = analyzeBody instance expr1
      let! expr2 = analyzeBody instance expr2
      return Combine(instance.CompiledName, expr1, expr2)
    }
  | ("Source", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> Source(instance.CompiledName, value))
  | ("Delay", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> Delay(instance.CompiledName, value))
  | ("Run", [expr]) ->
    analyzeBody instance expr
    |> AnalysisResult.map (fun value -> Run(instance.CompiledName, value))
  | _ ->
    result {
      let! args =
        exprs
        |> List.map (analyzeBody instance)
        |> AnalysisResult.sequence
      return ExpressionCall(Some (Value instance.CompiledName), member'.CompiledName, args)
    }
| BasicPatterns.Call(receiver, member', _, _, args) ->
  result {
    let! receiver =
      match receiver with
      | None -> Success None
      | Some r -> analyzeBody instance r |> AnalysisResult.map Some
    let! args =
      args
      |> List.map (analyzeBody instance)
      |> AnalysisResult.sequence
    return ExpressionCall(receiver, member'.DisplayName, args)
  }
| BasicPatterns.Sequential(expr1, expr2) ->
  result {
    let! expr1 = analyzeBody instance expr1
    let! expr2 = analyzeBody instance expr2
    return Sequential(expr1, expr2)
  }
| BasicPatterns.IfThenElse(cond, expr1, expr2) ->
  result {
    let! cond = analyzeBody instance cond
    let! expr1 = analyzeBody instance expr1
    let! expr2 = analyzeBody instance expr2
    return IfThenElse(cond, expr1, expr2)
  }
| BasicPatterns.Quote(expr) ->
  analyzeBody instance expr |> AnalysisResult.map Quote
| expr -> Failure [ sprintf "not supported: %A" expr ]

let private (|Builder|_|) = function
| BasicPatterns.Call(opt, memberOrFuncOrValue, _, _, args) ->
  match opt with
  | Some expr -> args |> List.map (sprintf "%A") |> String.concat " " |> sprintf "%s %s" memberOrFuncOrValue.DisplayName
  | None -> memberOrFuncOrValue.CompiledName
  |> Some
| _ -> None 

let private analyzeComputationExpression = function
| BasicPatterns.Application(BasicPatterns.Lambda(value, expr), _, [Builder instance]) ->
  match analyzeBody value expr with
  | Success body -> Success({ Instance = instance; Arg = value.CompiledName; Body = body })
  | Failure msgs -> Failure msgs
| expr -> Failure [ sprintf "Failed to analysis: %A" expr ]

let private analyzeDeclarations decls =
  match decls with
  | [FSharpImplementationFileDeclaration.Entity(_, subDecls)] ->
    match List.rev subDecls with
    | FSharpImplementationFileDeclaration.InitAction(expr) :: _ ->
      analyzeComputationExpression expr
    | _ -> Failure ["Last expression require `do computation-expression`."]
  | _ -> Failure ["ComVu allows one entity declaration."]

let private seqBuilder = """open FSharp.Core.CompilerServices

type SeqBuilder() =
  member __.Yield(x) = Seq.singleton x
  member __.YieldFrom(xs: _ seq) = xs
  member __.Return(()) = Seq.empty
  member __.Zero() = Seq.empty
  member __.Combine(xs1, f) = Seq.append xs1 (f ())
  member __.For(xs, g) = Seq.collect g xs
  member __.While(guard, body) = RuntimeHelpers.EnumerateWhile guard (body ())
  member __.TryFinally(xs, compensation) =
    RuntimeHelpers.EnumerateThenFinally xs compensation
  member __.Using(resource, xs) =
    RuntimeHelpers.EnumerateUsing resource xs
  member __.Delay(f) = f
  member __.Run(f) = f ()

let seq = SeqBuilder()
"""

let analyze input =
  async {
    let input = sprintf "%s%s%s" seqBuilder Environment.NewLine input
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
    File.WriteAllText(file, input)
    let! options = checker.GetProjectOptionsFromScript(file, input)
    let! result = checker.ParseAndCheckProject(options)
    return
      match result.Errors with
      | [||] ->
        let checkedFile = result.AssemblyContents.ImplementationFiles.[0]
        analyzeDeclarations checkedFile.Declarations
      | _ ->
        let errors = result.Errors |> Array.map (fun x -> x.Message) |> Array.toList
        Failure errors
  }
