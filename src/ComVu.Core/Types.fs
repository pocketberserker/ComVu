namespace ComVu

open System
open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat

[<AutoOpen>]
module private FormatHelper =

  let bracketL l = wordL "(" -- l @@ wordL ")"

  let methodArgs xs = bracketL (sepListL (wordL ",") xs)

type ComputationExpressionBody =
  | Const of string
  | Value of string
  | NewObject of ComputationExpressionBody list
  | Return of string * ComputationExpressionBody
  | Yield of string * ComputationExpressionBody
  | Zero of string
  | ReturnBang of string * ComputationExpressionBody
  | YieldBang of  string * ComputationExpressionBody
  | Lambda of string * ComputationExpressionBody
  | Let of string * ComputationExpressionBody * ComputationExpressionBody
  | ExpressionCall of ComputationExpressionBody option * string * ComputationExpressionBody list
  | LetBang of string * ComputationExpressionBody * ComputationExpressionBody
  | Use of string * ComputationExpressionBody * ComputationExpressionBody
  | While of string * ComputationExpressionBody * ComputationExpressionBody
  | For of string * ComputationExpressionBody * ComputationExpressionBody
  | TryWith of string * ComputationExpressionBody * ComputationExpressionBody
  | TryFinally of string * ComputationExpressionBody * ComputationExpressionBody
  | Sequential of ComputationExpressionBody * ComputationExpressionBody
  | Quote of ComputationExpressionBody
  | Source of string * ComputationExpressionBody
  | Delay of string * ComputationExpressionBody
  | Run of string * ComputationExpressionBody
with
  member this.Doc =
    match this with
    | Const v
    | Value v -> wordL v
    | NewObject args ->
      let args =
        args
        |> List.map (fun x -> x.ToString())
      if List.isEmpty args || args = ["()"] then bracketL emptyL
      else args |> List.map wordL |> tupleL
    | Zero instance -> wordL instance ^^ wordL ".Zero()"
    | Return(instance, arg) -> wordL instance ^^ wordL ".Return" ^^ methodArgs [arg.Doc]
    | ReturnBang(instance, arg) -> wordL instance ^^ wordL ".ReturnFrom" ^^ methodArgs [arg.Doc]
    | Yield(instance, arg) -> wordL instance ^^ wordL ".Yield" ^^ methodArgs [arg.Doc]
    | YieldBang(instance, arg) -> wordL instance ^^ wordL ".YieldFrom" ^^ methodArgs [arg.Doc]
    | Lambda(arg, body) -> wordL "fun" ^^ wordL arg ^^ wordL "->" -- body.Doc
    | Let(name, value, body) -> wordL "let" ^^ wordL name ^^ wordL "=" ^^ value.Doc @@ body.Doc
    | ExpressionCall(receiver, name, args) ->
      let receiver = match receiver with | Some x -> x.ToString() + "." |> wordL | None -> emptyL
      let args = args |> List.map objL |> methodArgs
      receiver ^^ wordL name ^^ args
    | LetBang(instance, src, lambda) ->
      wordL instance ^^ wordL ".Bind" ^^ methodArgs [src.Doc; lambda.Doc]
    | Use(instance, src, lambda) ->
      wordL instance ^^ wordL ".Using" ^^ methodArgs [src.Doc; lambda.Doc]
    | While(instance, cond, body) ->
      wordL instance ^^ wordL ".While" ^^ methodArgs [cond.Doc; body.Doc]
    | For(instance, src, lambda) ->
      wordL instance ^^ wordL ".For" ^^ methodArgs [src.Doc; lambda.Doc]
    | TryWith(instance, src, rescue) ->
      wordL instance ^^ wordL ".TryWith" ^^ methodArgs [src.Doc; rescue.Doc]
    | TryFinally(instance, src, expr) ->
      wordL instance ^^ wordL ".TryFinally" ^^ methodArgs [src.Doc; expr.Doc]
    | Sequential(expr1, expr2) -> expr1.Doc ^^ wordL ";" @@ expr2.Doc
    | Quote(expr) -> wordL "<@" -- expr.Doc @@ wordL "@>"
    | Source(instance, expr) -> wordL instance ^^ wordL ".Source" ^^ methodArgs [expr.Doc]
    | Delay(instance, expr) -> wordL instance ^^ wordL ".Delay" ^^ methodArgs [expr.Doc]
    | Run(instance, expr) -> wordL instance ^^ wordL ".Run" ^^ methodArgs [expr.Doc]
  override this.ToString() = print 2 this.Doc

type ComputationExpression = {
  Instance: string
  Arg: string
  Body: ComputationExpressionBody
}
with
  member this.Doc = wordL "(fun" ^^ wordL this.Arg ^^ wordL "->" -- this.Body.Doc @@ wordL ")" ^^ wordL this.Instance
  override this.ToString() = print 2 this.Doc

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
