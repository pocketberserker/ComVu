module ComVu.Analyzer

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
| expr -> Failure ["failed to optimize Bind lambda term."; sprintf "%O" expr ]

let rec private analysisBody instance = function
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
  List.foldBack (fun x rs ->
    AnalysisResult.bind (fun rs -> analysisBody instance x |> AnalysisResult.map (fun r -> r::rs)) rs
  ) args (Success [])
  |> AnalysisResult.map NewObject
| BasicPatterns.Lambda(args, expr) ->
  analysisBody instance expr
  |> AnalysisResult.map (fun body -> Lambda(displayArgs args, body))
| BasicPatterns.Let((name, value), body) ->
  result {
    let! value = analysisBody instance value
    let! body = analysisBody instance body
    return Let(name.CompiledName, value, body)
  }
| BasicPatterns.Call(BuilderInstance instance, member', _, _, exprs) ->
  match (member'.CompiledName, exprs) with
  | ("Zero", [_]) -> Success(Zero instance.CompiledName)
  | ("Return", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> Return(instance.CompiledName, value))
  | ("Yield", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> Yield(instance.CompiledName, value))
  | ("ReturnFrom", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> ReturnBang(instance.CompiledName, value))
  | ("YieldFrom", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> YieldBang(instance.CompiledName, value))
  | ("Bind", [src; lambda]) ->
    result {
      let! src = analysisBody instance src
      let! lambda = analysisBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return LetBang(instance.CompiledName, src, lambda)
    }
  | ("Using", [src; lambda]) ->
    result {
      let! src = analysisBody instance src
      let! lambda = analysisBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return Use(instance.CompiledName, src, lambda)
    }
  | ("While", [cond; body]) ->
    result {
      let! cond = analysisBody instance cond
      let! body = analysisBody instance body
      return While(instance.CompiledName, cond, body)
    }
  | ("For", [src; lambda]) ->
    result {
      let! src = analysisBody instance src
      let! lambda = analysisBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return For(instance.CompiledName, src, lambda)
    }
  | ("TryWith", [src; lambda]) ->
    result {
      let! src = analysisBody instance src
      let! lambda = analysisBody instance lambda
      let! lambda = optimizeBindLambdaArg lambda
      return TryWith(instance.CompiledName, src, lambda)
    }
  | ("TryFinally", [src; lambda]) ->
    result {
      let! src = analysisBody instance src
      let! lambda = analysisBody instance lambda
      return TryFinally(instance.CompiledName, src, lambda)
    }
  | ("Combine", [expr1; expr2]) ->
    result {
      let! expr1 = analysisBody instance expr1
      let! expr2 = analysisBody instance expr2
      return Combine(instance.CompiledName, expr1, expr2)
    }
  | ("Source", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> Source(instance.CompiledName, value))
  | ("Delay", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> Delay(instance.CompiledName, value))
  | ("Run", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.map (fun value -> Run(instance.CompiledName, value))
  | _ -> Failure [ sprintf "not supported: %s" member'.CompiledName ]
| BasicPatterns.Call(receiver, member', _, _, args) ->
  result {
    let! receiver =
      match receiver with
      | None -> Success None
      | Some r -> analysisBody instance r |> AnalysisResult.map Some
    let! args =
      List.foldBack (fun x rs ->
        AnalysisResult.bind (fun rs -> analysisBody instance x |> AnalysisResult.map (fun r -> r::rs)) rs
      ) args (Success [])
    return ExpressionCall(receiver, member'.DisplayName, args)
  }
| BasicPatterns.Sequential(expr1, expr2) ->
  result {
    let! expr1 = analysisBody instance expr1
    let! expr2 = analysisBody instance expr2
    return Sequential(expr1, expr2)
  }
| BasicPatterns.IfThenElse(cond, expr1, expr2) ->
  result {
    let! cond = analysisBody instance cond
    let! expr1 = analysisBody instance expr1
    let! expr2 = analysisBody instance expr2
    return IfThenElse(cond, expr1, expr2)
  }
| BasicPatterns.Quote(expr) ->
  analysisBody instance expr |> AnalysisResult.map Quote
| expr -> Failure [ sprintf "not supported: %A" expr ]

let private (|Builder|_|) = function
| BasicPatterns.Call(opt, memberOrFuncOrValue, _, _, args) ->
  match opt with
  | Some expr -> args |> List.map (sprintf "%A") |> String.concat " " |> sprintf "%s %s" memberOrFuncOrValue.DisplayName
  | None -> memberOrFuncOrValue.CompiledName
  |> Some
| _ -> None 

let private analysisComputationExpression = function
| BasicPatterns.Application(BasicPatterns.Lambda(value, expr), _, [Builder instance]) ->
  match analysisBody value expr with
  | Success body -> Success({ Instance = instance; Arg = value.CompiledName; Body = body })
  | Failure msgs -> Failure msgs
| expr -> Failure [ sprintf "Failed to analysis: %A" expr ]

let private analysisDeclarations decls =
  match decls with
  | [FSharpImplementationFileDeclaration.Entity(_, subDecls)] ->
    match List.rev subDecls with
    | FSharpImplementationFileDeclaration.InitAction(expr) :: _ ->
      analysisComputationExpression expr
    | _ -> Failure ["Last expression require `do computation-expression`."]
  | _ -> Failure ["ComVu allows one entity declaration."]

let analysis input =
  async {
    let file = Path.ChangeExtension(Path.GetTempFileName(), "fsx")
    File.WriteAllText(file, input)
    let! options = checker.GetProjectOptionsFromScript(file, input)
    let! result = checker.ParseAndCheckProject(options)
    return
      match result.Errors with
      | [||] ->
        let checkedFile = result.AssemblyContents.ImplementationFiles.[0]
        analysisDeclarations checkedFile.Declarations
      | _ ->
        let errors = result.Errors |> Array.map (fun x -> x.Message) |> Array.toList
        Failure errors
  }
