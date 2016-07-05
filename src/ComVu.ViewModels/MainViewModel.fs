namespace ComVu.ViewModels

open System
open Reactive.Bindings
open System.Reactive.Linq
open ComVu

type MainViewModel() =

  let mutable code = new ReactiveProperty<string>()
  let mutable analysis = code.Select(not << String.IsNullOrEmpty).ToReactiveCommand()
  let output = new ReactiveProperty<string>("")

  do
    analysis
      .Subscribe(fun _ ->
        output.Value <-
          match Analyzer.analysis code.Value |> Async.RunSynchronously with
          | Success v ->v.ToString()
          | Failure msgs -> msgs |> String.concat Environment.NewLine
      )
    |> ignore

  member __.Code with get() = code and set(value) = code <- value
  member __.Analysis with get() = analysis and set(value) = analysis <- value
  member __.Output = output
