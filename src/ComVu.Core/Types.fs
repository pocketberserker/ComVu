namespace ComVu

open System

type ComputationExpressionBody =
  | Const of string
  | Return of instance: string * ComputationExpressionBody
  | Yield of instance: string * ComputationExpressionBody
  | ReturnFrom of instance: string * ComputationExpressionBody
  | YieldFrom of instance: string * ComputationExpressionBody
  | Lambda of args: string * ComputationExpressionBody
  | Quote of ComputationExpressionBody
  | Delay of instance: string * ComputationExpressionBody
  | Run of instance: string * ComputationExpressionBody
with
  override this.ToString() =
    match this with
    | Const v -> v
    | Return(instance, arg) -> sprintf "%s.Return(%O)" instance arg
    | ReturnFrom(instance, arg) -> sprintf "%s.ReturnFrom(%O)" instance arg
    | Yield(instance, arg) -> sprintf "%s.Yield(%O)" instance arg
    | YieldFrom(instance, arg) -> sprintf "%s.YieldFrom(%O)" instance arg
    | Lambda(arg, body) -> sprintf "fun %s ->%s%O" arg Environment.NewLine body
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
    [ sprintf "fun %s ->" this.Arg; this.Body.ToString() ]
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
