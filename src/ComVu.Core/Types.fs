namespace ComVu

open System

type ComputationExpressionBody =
  | Const of string
  | Value of string
  | NewObject of ComputationExpressionBody list
  | Return of string * ComputationExpressionBody
  | Yield of string * ComputationExpressionBody
  | ReturnBang of string * ComputationExpressionBody
  | YieldBang of  string * ComputationExpressionBody
  | Lambda of string * ComputationExpressionBody
  | Let of string * ComputationExpressionBody * ComputationExpressionBody
  | ExpressionCall of ComputationExpressionBody option * string * ComputationExpressionBody list
  | LetBang of string * ComputationExpressionBody * ComputationExpressionBody
  | Use of string * ComputationExpressionBody * ComputationExpressionBody
  | Quote of ComputationExpressionBody
  | Delay of string * ComputationExpressionBody
  | Run of string * ComputationExpressionBody
with
  override this.ToString() =
    match this with
    | Const v
    | Value v -> v
    | NewObject args ->
      let args =
        args
        |> List.map (fun x -> x.ToString())
        |> String.concat ("," + Environment.NewLine)
      if String.IsNullOrEmpty(args) || args = "()" then "()"
      else sprintf "(%s)" args
    | Return(instance, arg) -> sprintf "%s.Return(%O)" instance arg
    | ReturnBang(instance, arg) -> sprintf "%s.ReturnFrom(%O)" instance arg
    | Yield(instance, arg) -> sprintf "%s.Yield(%O)" instance arg
    | YieldBang(instance, arg) -> sprintf "%s.YieldFrom(%O)" instance arg
    | Lambda(arg, body) -> sprintf "fun %s ->%s%O" arg Environment.NewLine body
    | Let(name, value, body) -> sprintf "let %s = %O in %O" name value body
    | ExpressionCall(instance, name, args) ->
      let receiver =
        match instance with
        | Some i -> i.ToString() + "."
        | None -> ""
      let args =
        args
        |> List.map (fun x -> x.ToString())
        |> String.concat ("," + Environment.NewLine)
      let args = if String.IsNullOrEmpty(args) then "" else args + Environment.NewLine
      sprintf "%s%s(%s)" receiver name args
    | LetBang(instance, src, lambda) ->
      sprintf "%s.Bind(%s%O,%s%O%s)" instance Environment.NewLine src Environment.NewLine lambda Environment.NewLine
    | Use(instance, src, lambda) ->
      sprintf "%s.Using(%s%O,%s%O%s)" instance Environment.NewLine src Environment.NewLine lambda Environment.NewLine
    | Quote(expr) -> sprintf "<@%s%O%s@>" Environment.NewLine expr Environment.NewLine
    | Delay(instance, expr) -> sprintf "%s.Delay(%s%O%s)" instance Environment.NewLine expr Environment.NewLine
    | Run(instance, expr) -> sprintf "%s.Run(%s%O%s)" instance Environment.NewLine expr Environment.NewLine

type ComputationExpression = {
  Instance: string
  Arg: string
  Body: ComputationExpressionBody
}
with
  override this.ToString() =
    [ sprintf "(fun %s ->" this.Arg; this.Body.ToString(); sprintf ") %s" this.Instance ]
    |> String.concat Environment.NewLine

type AnalysisResult<'T> =
  | Success of 'T
  | Failure of string list

module AnalysisResult =

  let map f = function
  | Success v -> Success (f v)
  | Failure msgs -> Failure msgs

  let bind f = function
  | Success v -> f v
  | Failure msgs -> Failure msgs

type AnalysisResultBuilder internal () =
  member inline __.Bind(x, f) = AnalysisResult.bind f x
  member __.Return(x) = Success x
  member inline __.ReturnFrom(x: AnalysisResult<_>) = x

[<AutoOpen>]
module AnslysisResultSyntax =

  let result = AnalysisResultBuilder()
