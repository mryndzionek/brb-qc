# brb-qc - testing stateful, embedded C code from Haskell using QuickCheck

Property-based testing is somewhat more scientific approach to testing.
Instead of writing test cases manually, programmer only states her assumptions about the code and delegates
tedious task of falsifying the hypothesis to a computer. This is a better use of time and effort
than in traditional TDD.

QuickCheck is state of the art library for property based testing written in Haskell.
It's more convenient than similar libraries in other languages, as it's designed as a simple DSL.
Monadic interface plays well with testing stateful and "impure" code in general. Built-in FFI support
makes specially easy to call C code directly.
