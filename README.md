# Logic programming in SystemFC/Haskell Type System

This is just an example/test of encoding logic/Prolog algorithms with the
Haskell Type System.

Example 2 doesn't work. But I suspect it is because of the functional
dependency on the class declaration.

Example 3 is using type families with Peano's axioms. While it is
obviously possible to do it with classes (as in Prolog), I didn't
do it like that for no particular reason. But `Factorial` works
as it is.

```bash
ivanmolinarebolledo@Ivans-macOS hst % ghci -V
The Glorious Glasgow Haskell Compilation System, version 9.2.1
```