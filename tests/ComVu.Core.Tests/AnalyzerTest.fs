module ComVu.Core.Tests.AnalyzerTest

open System.IO
open Persimmon
open UseTestNameByReflection
open ComVu

let zeroOnly =
  let source = """type TestBuilder() =
  member __.Zero() = 0

let test = TestBuilder()

test { () }"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = Sequential(Const "()", Zero "builder@")
  }
  (source, expected)

let returnOnly =
  let source = """type TestBuilder() =
  member __.Return(x) = x

let test = TestBuilder()

test {
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = Return("builder@", Const "0")
  }
  (source, expected)

let yieldOnly =
  let source = """type TestBuilder() =
  member __.Yield(x) = x

let test = TestBuilder()

test {
  yield 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = Yield("builder@", Const "0")
  }
  (source, expected)

let returnBang =
  let source = """type TestBuilder() =
  member __.ReturnFrom(x) = x

let test = TestBuilder()

test {
  return! 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = ReturnBang("builder@", Const "0")
  }
  (source, expected)

let yieldBang =
  let source = """type TestBuilder() =
  member __.YieldFrom(x) = x

let test = TestBuilder()

test {
  yield! 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = YieldBang("builder@", Const "0")
  }
  (source, expected)

let letReturn =
  let source = """type TestBuilder() =
  member __.Return(x) = x

let test = TestBuilder()

test {
  let x = 0
  return x + 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Let(
        "x",
        Const "0",
        Return(
          "builder@",
          ExpressionCall(None, "( + )", [Value "x"; Const "1"])
        )
      )
  }
  (source, expected)

let letBang =
  let source = """type TestBuilder() =
  member __.Return(x) = x
  member __.Bind(x, f) = f x

let test = TestBuilder()

test {
  let! x = 0
  return x + 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      LetBang(
        "builder@",
        Const "0",
        Lambda(
          "x",
          Return(
            "builder@",
            ExpressionCall(None, "( + )", [Value "x"; Const "1"])
          )
        )
      )
  }
  (source, expected)

let doReturn =
  let source = """type TestBuilder() =
  member __.Return(x) = x

let test = TestBuilder()

test {
  do ()
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Sequential(
        Const "()",
        Return(
          "builder@",
          Const "0"
        )
      )
  }
  (source, expected)

let disposable = """open System

type Disposable() =
  interface IDisposable with
    member this.Dispose() = ()
"""

let useReturn =
  let source = disposable + """
type TestBuilder() =
  member __.Return(x) = x
  member this.Using(x: #IDisposable, f) =
    try f x
    finally match box x with null -> () | notNull -> x.Dispose()

let test = TestBuilder()

test {
  use x = new Disposable()
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Use(
        "builder@",
        NewObject([Const "()"]),
        Lambda(
          "x",
          Return(
            "builder@",
             Const "0"
          )
        )
      )
  }
  (source, expected)

let useBang =
  let source = disposable + """
type TestBuilder() =
  member __.Return(x) = x
  member this.Using(x: #IDisposable, f) =
    try f x
    finally match box x with null -> () | notNull -> x.Dispose()
  member __.Bind(x, f) = f x

let test = TestBuilder()

test {
  use! x = new Disposable()
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      LetBang(
        "builder@",
        NewObject([Const "()"]),
        Lambda(
          "x",
          Use(
            "builder@",
            Value "x",
            Lambda(
              "x",
              Return(
                "builder@",
                 Const "0"
              )
            )
          )
        )
      )
  }
  (source, expected)

let whileReturn =
  let source = """type TestBuilder() =
  member __.While(cond, f) = if cond () then f () else Unchecked.defaultof<_>
  member __.Return(x) = x
  member __.Delay(f) = f
  member __.Run(f) = f ()

let test = TestBuilder()

test {
  while true do
    return 0
}"""
  let expected = {
    Instance = "test"
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
                Lambda("()", Return("builder@", Const "0"))
              )
            )
          )
        )
      )
  }
  (source, expected)

let forReturn =
  let source = """type TestBuilder() =
  member __.Return(x) = x
  member __.For(x, f) = f x

let test = TestBuilder()

test {
  for x in 0 do
    return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      For(
        "builder@",
        Const "0",
        Lambda(
          "x",
          Return(
            "builder@",
            Const "0"
          )
        )
      )
  }
  (source, expected)

let tryWith =
  let source = """type TestBuilder() =
  member __.Return(x) = x
  member __.TryWith(x, g) =
    try
      x
    with e ->
      g e
  member __.Delay(f) = f ()

let test = TestBuilder()

test {
  try
    return 0
  with e ->
    return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Delay(
        "builder@",
        Lambda(
          "()",
          TryWith(
            "builder@",
            Delay(
              "builder@",
              Lambda("()", Return("builder@", Const "0"))
            ),
            Lambda(
              "e",
              Return(
                "builder@",
                Const "0"
              )
            )
          )
        )
      )
  }
  (source, expected)

let tryFinally =
  let source = """type TestBuilder() =
  member __.Return(x) = x
  member __.TryFinally(x, g) =
    try
      x
    finally
      g ()
  member __.Delay(f) = f ()

let test = TestBuilder()

test {
  try
    return 0
  finally
    ()
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Delay(
        "builder@",
        Lambda(
          "()",
          TryFinally(
            "builder@",
            Delay(
              "builder@",
              Lambda("()", Return("builder@", Const "0"))
            ),
            Lambda(
              "()",
              Const "()"
            )
          )
        )
      )
  }
  (source, expected)

let sequential =
  let source = """type TestBuilder() =
  member __.Yield(x) = x
  member __.Combine(x, y) = y
  member __.Delay(f) = f ()

let test = TestBuilder()

test {
  yield 0
  yield 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      Delay(
        "builder@",
        Lambda(
          "()",
          Combine(
            "builder@",
            Yield("builder@", Const "0"),
            Delay(
              "builder@",
              Lambda(
                "()",
                Yield("builder@", Const "1")
              )
            )
          )
        )
      )
  }
  (source, expected)

let ifThen =
  let source = """type TestBuilder() =
  member __.Return(x) = x
  member __.Zero() = 0

let test = TestBuilder()

test {
  if true then
    return 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      IfThenElse(
        Const "True",
        Return("builder@", Const "1"),
        Zero "builder@"
      )
  }
  (source, expected)

let ifThenElse =
  let source = """type TestBuilder() =
  member __.Return(x) = x

let test = TestBuilder()

test {
  if true then
    return 0
  else
    return 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      IfThenElse(
        Const "True",
        Return("builder@", Const "0"),
        Return("builder@", Const "1")
      )
  }
  (source, expected)

let defineSource =
  let source = """type TestBuilder() =
  member __.ReturnFrom(x) = x
  member __.Source(x) = x

let test = TestBuilder()

test {
  return! 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = ReturnBang("builder@", Source("builder@", Const "0"))
  }
  (source, expected)

let quote =
  let source = """type TestBuilder() =
  member __.Quote() = ()
  member __.Return(x) = x

let test = TestBuilder()

test {
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = Quote(Return("builder@", Const "0"))
  }
  (source, expected)

let externalLibrary =
  let rootDir = Path.Combine(__SOURCE_DIRECTORY__, "../..")
  let dll =
#if Debug
    "Debug"
#else
    "Release"
#endif
    |> sprintf "tests/TestLibrary/bin/%s/TestLibrary.dll"
  let loadDll =
    Path.Combine(rootDir, dll)
    |> sprintf "#r @\"%s\""
  let source = loadDll + """

open TestLibrary

let test = TestBuilder()

test {
  let! x = 0
  return x + 1
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body =
      LetBang(
        "builder@",
        Const "0",
        Lambda(
          "x",
          Return(
            "builder@",
            ExpressionCall(None, "( + )", [Value "x"; Const "1"])
          )
        )
      )
  }
  (source, expected)

let ``analysis computation expression`` = parameterize {
  source [
    zeroOnly
    returnOnly
    yieldOnly
    returnBang
    yieldBang
    letReturn
    letBang
    doReturn
    useReturn
    useBang
    whileReturn
    forReturn
    tryWith
    tryFinally
    sequential
    ifThen
    ifThenElse
    defineSource
    quote
    externalLibrary
  ]
  run (fun (source, expected) -> test {
    let! actual = asyncRun { it (Analyzer.analysis source) }
    do! assertEquals (Success expected) actual
  })
} 
