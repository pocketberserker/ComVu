# ComVu

[![Build status](https://ci.appveyor.com/api/projects/status/n8hmjy24a5t96g4i/branch/master?svg=true)](https://ci.appveyor.com/project/pocketberserker/comvu/branch/master)

ComVu is a computation expressions visualyzer.
This tool can analysis your computation expressions.

## Features

- Conversion rule
  - [x] ``let p = e in ce``
  - [x] ``let! p = e in ce``
  - [x] ``yield e``
  - [x] ``yield! e``
  - [x] ``return e``
  - [x] ``return! e``
  - [x] ``use p = e in ce``
  - [x] ``use! p = e in ce``
  - [x] ``while e do ce``
  - [x] ``try ce with pi -> cei``
  - [x] ``try ce finally e``
  - [ ] ``if e then ce``
  - [ ] ``if e then ce1 else ce2``
  - [x] ``for x in e do ce``
  - [x] ``do e in ce``
  - [x] ``do! e in ce``
  - [ ] ``ce1; ce2``
  - [x] ``do! e;``
  - [x] ``e;``
- [ ] Sequence expression like seq computation expression
- [ ] External function or method call
- [ ] External libraries

- Not support
  - ``match e with pi -> cei``
  - Custom operator
  - ``joinOp``, ``groupJoinOp``

