module TestLibrary

type TestBuilder() =
  member __.Bind(x, f) = f x
  member __.Return(x) = x

let test = TestBuilder()
