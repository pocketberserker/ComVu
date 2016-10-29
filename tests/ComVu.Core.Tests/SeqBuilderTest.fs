module ComVu.Core.Tests.SeqBuilderTest

open Persimmon
open UseTestNameByReflection
open ComVu

let yieldOnly =
  let source = """seq {
  yield 0
}"""
  let expected = {
    Instance = "seq"
    Arg = "builder@"
    Body =
      Run(
        "builder@",
        Delay(
          "builder@",
          Lambda(
            "()",
            Yield("builder@", Const "0")
          )
        )
      )
  }
  (source, expected)

let yieldBang =
  let source = """seq {
  yield! [0]
}"""
  let expected = {
    Instance = "seq"
    Arg = "builder@"
    Body =
      Run(
        "builder@",
        Delay(
          "builder@",
          Lambda(
            "()",
            YieldBang(
              "builder@",
              NewUnionCase(
                "op_ColonColon",
                [
                  Const "0"
                  NewUnionCase("op_Nil", [])
                ]
              )
            )
          )
        )
      )
  }
  (source, expected)

let disposable = """open System

type Disposable() =
  interface IDisposable with
    member this.Dispose() = ()
"""

let useYield =
  let source = disposable + """seq {
  use x = new Disposable()
  yield 0
}"""
  let expected = {
    Instance = "seq"
    Arg = "builder@"
    Body =
      Run(
        "builder@",
        Delay(
          "builder@",
          Lambda(
            "()",
            Use(
              "builder@",
              NewObject([Const "()"]),
              Lambda(
                "x",
                Yield(
                  "builder@",
                   Const "0"
                )
              )
            )
          )
        )
      )
  }
  (source, expected)

let whileYield =
  let source = """seq {
  while true do
    yield 0
}"""
  let expected = {
    Instance = "seq"
    Arg = "builder@"
    Body =
      Run(
        "builder@",
        Delay(
          "builder@",
          Lambda(
            "()",
            While(
              "builder@",
              Lambda("()", Const "True"),
              Delay(
                "builder@",
                Lambda("()", Yield("builder@", Const "0"))
              )
            )
          )
        )
      )
  }
  (source, expected)

let forYield =
  let source = """seq {
  for x in [0] do
    yield 0
}"""
  let expected = {
    Instance = "seq"
    Arg = "builder@"
    Body =
      Run(
        "builder@",
        Delay(
          "builder@",
          Lambda(
            "()",
            For(
              "builder@",
              NewUnionCase(
                "op_ColonColon",
                [
                  Const "0"
                  NewUnionCase("op_Nil", [])
                ]
              ),
              Lambda(
                "x",
                Yield(
                  "builder@",
                  Const "0"
                )
              )
            )
          )
        )
      )
  }
  (source, expected)

let ``analysis seq computation expression(sequence expression like)`` = parameterize {
  source [
    yieldOnly
    yieldBang
    useYield
    whileYield
    forYield
  ]
  run (fun (source, expected) -> test {
    let! actual = asyncRun { it (Analyzer.analyze source) }
    do! assertEquals (Success expected) actual
  })
} 
