[![Build Status](https://secure.travis-ci.org/sboosali/vinyl-fields.svg)](http://travis-ci.org/sboosali/vinyl-fields)
[![Hackage](https://img.shields.io/hackage/v/vinyl-fields.svg)](https://hackage.haskell.org/package/vinyl-fields)

# vinyl-fields

vinyl records that are specialized to take a set of key-value pairs (i.e. `(Symbol,*)`), which means that each field has a name and a type. 

the motivation was writing a `FromJSON` instance for records. but I got too many kind errors, whether I represented the "record" (i.e. JavaScript object or Python dict), as `Rec (f :. ElField) ['("a",String), '("b",Bool)]` or, like most other vinyl libraries, as `Rec f [ElField '("a",String), ElField '("b",Bool)]`.

while the representation should be possible, it's still not convenient. I've been exclusively using `vinyl` with `Symbol`, so this specialization should only help, both with writing record functions and with the error messages. also, once GHC gets actual records (through some particular language extension, a type system plug-in, dependent types, or whatever), i'll want to keep the style of a type level set (i.e. `MAP Symbol *`) anyways.

