{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-} 

{-| 

-}
module Vinyl.Fields.TypeLevel where
import Prelude
import GHC.TypeLits

{-|

see https://ghc.haskell.org/trac/ghc/wiki/Proposal/CustomTypeErrors

-}
class ZipFields
  (fs :: [(Symbol, (k,j))])
  (as :: [(Symbol,     k)])
  (bs :: [(Symbol,     j)])
  where 

instance ZipFields '[] '[] '[] where

{-
instance (sa ~ sb, ZipFields fs as bs) => ZipFields
  ('(sa, '(a,b)) ': fs)
  ('(sa,      a) ': as)
  ('(sb,      b) ': bs)
  where 
-}
 
instance (ZipFields fs as bs) => ZipFields
  ('(s, '(a,b)) : fs)
  ('(s,      a) : as)
  ('(s,      b) : bs)
  where 

{-NOTE

OverloadedInstances excludes the KeysMismatch instance. 

with type equality, the error is readable but not optimal: 

    * Couldn't match type `"a"' with `"x"'
        arising from a use of `example_KeysMismatch_ZipFields'

-}

    
instance {-# OVERLAPS #-} (TypeError
     ( Text "[ZipFields _ as bs] all keys must match for zipped records, but:" 
  :$$: Text "  "
  :<>: ShowType sa
  :<>: Text " doesn't match "
  :<>: ShowType sb
  :<>: Text "."
  :$$: Text "where the rest of the records at the mismatched key is:"
  :$$: Text "  `as` ~ "
  :<>: ShowType ('(sa,a) : as)
  :$$: Text "  `bs` ~ "
  :<>: ShowType ('(sb,b) : bs)
     )) => ZipFields
  (fs)
  ('(sa,a) : as)
  ('(sb,b) : bs)

-- instance (TypeError
--      ( Text "[ZipFields _ as bs] all keys must match for zipped records, but: the `as` key "
--   :<>: ShowType sa
--   :<>: Text " doesn't match the `bs` key "
--   :<>: ShowType sb)
--      ) => ZipFields
--   (fs)
--   ('(sa,a) : as)
--   ('(sb,b) : bs)

instance (TypeError
     ( Text "[ZipFields _ as bs] zipped records must be the same length, but:"
  :$$: Text "`as` is empty while `bs` is still "
  :<>: ShowType (b : bs)
  :<>: Text "."
  :$$: Text "i.e. `bs` is longer by "
  :<>: ShowType (Length (b : bs)) 
  :<>: Text " fields."
     )) => ZipFields
  fs 
  '[]
  (b : bs)

-- -- instance (TypeError
-- --      ( Text "[ZipFields _ as bs] zipped records must be the same length, but: `as` is still "
-- --   :<>: ShowType (a : as)
-- --   :<>: Text "while `bs` is empty")
-- --      ) => ZipFields
-- --   fs 
-- --   (a : as)
-- --   '[]

type Example_Success_ZipFields = ZipFields
  ["a" ::: Integer ::: String, "b" ::: Bool ::: Char]
  ["a" ::: Integer           , "b" ::: Bool]
  ["a" ::: String            , "b" ::: Char]

type Example_KeysMismatch_ZipFields = ZipFields
  ["a" ::: Integer ::: String, "b" ::: Bool ::: Char]
  ["a" ::: Integer,            "b" ::: Bool]
  ["x" ::: String,             "b" ::: Char]

type Example_WrongLengths_ZipFields = ZipFields
  ["a" ::: Integer ::: String, "b" ::: Bool ::: Char]
  ["a" ::: Integer           , "b" ::: Bool]
  ["a" ::: String            , "b" ::: Char, "c" ::: Char]

-- ZipTypes '["a" ::: Integer, "b" ::: Bool] '["a" ::: String, "y" ::: Char]

{-|

succeeds with no type error. 

-}
example_Success_ZipFields :: (Example_Success_ZipFields) => ()
example_Success_ZipFields = ()

{-|

fails with this type error: 

@
   * Couldn't match type `"a"' with `"x"'
        arising from a use of `example_KeysMismatch_ZipFields'
@

-}
example_KeysMismatch_ZipFields :: (Example_KeysMismatch_ZipFields) => ()
example_KeysMismatch_ZipFields = ()

{-|

fails with this type error: 

@
    * [ZipFields _ as bs] zipped records must be the same length, but: `as` is empty while `bs` is still '['("c",
                                                                                                             Char)].
      i.e. `bs` is longer by 1 fields.
    * In the expression: example_WrongLengths_ZipFields
@

-}
example_WrongLengths_ZipFields :: (Example_WrongLengths_ZipFields) => ()
example_WrongLengths_ZipFields = () 

--------------------------------------------------------------------------------


{-| sugar for a type-level pair.

@
>>> :set 
>>> :set -XDataKinds
>>> type Dog f = Record f ["name" ::: String, "age" ::: Natural]
@

-}
type (:::) k a = '(k, a)
infixr 1 ::: 

{-| type-level @fmap@, for type-level lists only, poly kinded.

for example:

@
>>> :kind! Fmap Pair [Char,Bool,Int]
    :: [*]
    = '[(Char, Char), (Bool, Bool), (Int, Int)]
@

given:

@
-- type-level @(,)@ 
type family Pair (x :: *) where
  Pair x = (x, x)
@

for more details on Type Families, see https://kseo.github.io/posts/2017-01-16-type-level-functions-using-closed-type-families.html

-}
type family Fmap (f :: k -> j) (xs :: [k]) :: [j] where
   Fmap f '[]       = '[]
   Fmap f (x ': xs) = f x ': Fmap f xs

{-|

-}
type family Length (xs :: [k]) :: Nat where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs

