module ComVu.Core.Tests.AnalyzerTest

open Persimmon
open UseTestNameByReflection
open ComVu

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

let returnFrom =
  let source = """type TestBuilder() =
  member __.ReturnFrom(x) = x

let test = TestBuilder()

test {
  return! 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = ReturnFrom("builder@", Const "0")
  }
  (source, expected)

let yieldFrom =
  let source = """type TestBuilder() =
  member __.YieldFrom(x) = x

let test = TestBuilder()

test {
  yield! 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = YieldFrom("builder@", Const "0")
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

let delayRun =
  let source = """type TestBuilder() =
  member __.Delay(f) = f
  member __.Return(x) = x
  member __.Run(f) = f ()

let test = TestBuilder()

test {
  return 0
}"""
  let expected = {
    Instance = "test"
    Arg = "builder@"
    Body = Run("builder@", Delay("builder@", Lambda("()", Return("builder@", Const "0"))))
  }
  (source, expected)

let ``analysis computation expression`` = parameterize {
  source [
    returnOnly
    yieldOnly
    returnFrom
    yieldFrom
    quote
    delayRun
  ]
  run (fun (source, expected) -> test {
    let! actual = asyncRun { it (Analyzer.analysis source) }
    do! assertEquals (Success expected) actual
  })
} 
