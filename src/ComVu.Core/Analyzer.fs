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

let rec private analysisBody instance = function
| BasicPatterns.Const(v, _) -> Success(Const(v.ToString()))
| BasicPatterns.Lambda(args, expr) ->
  analysisBody instance expr
  |> AnalysisResult.map (fun body -> Lambda(displayArgs args, body))
| BasicPatterns.Call(BuilderInstance instance, member', _, _, exprs) ->
  match (member'.CompiledName, exprs) with
  | ("Return", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(Return(instance.CompiledName, value)))
  | ("Yield", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(Yield(instance.CompiledName, value)))
  | ("ReturnFrom", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(ReturnFrom(instance.CompiledName, value)))
  | ("YieldFrom", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(YieldFrom(instance.CompiledName, value)))
  | ("Delay", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(Delay(instance.CompiledName, value)))
  | ("Run", [expr]) ->
    analysisBody instance expr
    |> AnalysisResult.bind (fun value -> Success(Run(instance.CompiledName, value)))
  | _ -> Failure [ sprintf "not supported: %s" member'.CompiledName ]
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
