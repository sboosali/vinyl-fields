{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators, TypeApplications #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-} 
{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

{-| 

-}
module Vinyl.Fields.Types where
import Vinyl.Fields.Extra
import Vinyl.Fields.Core

import Data.Functor.Identity

import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Kind 
-- import Data.Functor.Compose 
import Data.Functor.Classes (Show1(..), showsPrec1) 
import Data.Constraint 

type Dictionary = Dict

--------------------------------------------------------------------------------

infixr 7 :*
infixr 1 ***

infix  2 -:
infix  2 =:

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

-- | the default precedence for application of alphanumeric functions
defaultPrecedence :: Int 
defaultPrecedence = 10

-- | must match the precedence of the @(':*)@ operator. for the custom 'Show' instance. 
precedenceOfRecordCons :: Int 
precedenceOfRecordCons = 7

-- | 
type Record_ f = Record '[] f 

-- | 

type Field_ k f = Field k '[] f 

type R = IRecord_ 

type F k = IField_ k

-- | 
type IRecord cs = Record cs Identity 

-- | 
type IField k cs = Field k cs Identity

-- | 
type PRecord cs = Record cs Proxy  

-- | 
type PField k cs = Field k cs Proxy 

-- | a record with a single constraint 
type RecordOf c f = Record '[c] f

-- | a field with a single constraint 
type FieldOf k c f = Field k '[c] f

-- | 
type IRecord_ = IRecord '[] 

-- | 
type IField_ k = IField k '[] 

-- | 
type PRecord_ = PRecord '[]  

-- | 
type PField_ k = PField k '[] 


field_ 
  :: forall (k :: Symbol). 
     forall f a. 
    ( KnownSymbol k
    ) 
  => f a
  -> Field_ k f a 
field_ = Field 

--------------------------------------------------------------------------------

{-| 

`fields` should be a type-level map (i.e. `Map Symbol *`), 
since we don't want duplicate field names, 
and since we don't care about the order of the keys,
but GHC currently only has type-level lists. 

@cs@ should be a type-level set (i.e. `Set (* -> Constraint)`), 
since we don't care about the order of the constraints 
(Haskell doesn't order @(C1 a, C2 a) => ...@,
they're all eventually applied).

@
>>> :set -XDataKinds
>>> type Dog f = Record f ['("name",String), '("age",Natural)]
>>> dog = (Identity "loki" :* Identity 7 :* R) :: Dog Identity
@

-}
data Record :: [k -> Constraint] 
            -> (k -> *) 
            -> [(Symbol,k)]
            -> *
    where

    R      :: Record cs f '[]

    (:*)   :: (Field k cs f a) 
           -> (Record cs f            fields) 
           -> (Record cs f ('(k,a) ': fields))

instance (AllTypes Show kvs, Show1 f) => Show (Record cs f kvs) where
  showsPrec = showsPrecedent_Record

showsPrecedent_Record 
  :: (AllTypes Show as, Show1 f) 
  => Showing (Record cs f as) 
showsPrecedent_Record d r = showParen (d `greaterThan` defaultPrecedence) $
   showsPrecedent_Record' d (reifyConstraint cShow r)
   where
   cShow = P @(Type -> Constraint) @Show
   -- P @(Type -> Constraint) @Show
   -- P @Show 

showsPrecedent_Record' 
  :: forall f cs as. (Show1 f) 
  => Showing (Record (Show ': cs) f as) 
showsPrecedent_Record' _d
  = mapRecordFields _show
  > recordToList 
  > (<> [s "R"])
  > intersperse (s " :* ") 
  > foldl (.) id
  where
  s = showString    
  _show :: forall k. forall x.
           Field k (Show ': cs) f         x
        -> Field k cs           (C ShowS) x
  _show f@(Field{}) = cfield @k $ 
      -- showParen (d `greaterThan` defaultPrecedence) $ 
          -- showsPrec d f -- (1+defaultPrecedence) f
      -- showParen (d `greaterThan` precedenceOfRecordCons) $ 
          showsPrec (1+precedenceOfRecordCons) f

-- | transform the functors, 
-- consuming the constraint and matching the field name. 
-- 
-- FieldTransformation k Show cs f (C ShowS) a
type FieldTransformation k c cs f g x
   = Field k (c ': cs) f x
  -> Field k cs        g x

-- FieldTransformation' Show f (C ShowS)
type FieldTransformation' c f g
   = (forall k cs x. FieldTransformation k c cs f g x)

-- | A record with uniform fields may be turned into a list.
recordToList
  :: Record cs (Const a) bs
  -> [a]
recordToList = \case
  R               -> []
  (Field x :* xs) -> getConst x : recordToList xs

{-| like @f@ composed with a @KnownSymbol@ dictionary. 
almost an @Identity@ functor.

pattern matching on a 'Field' exposes the constraints being stored. 

-}
data Field 
  :: Symbol    -- otherwise, TypeApplication doesn't work with the constructor 
  -> [k -> Constraint] 
  -> (k -> *) 
  -> k
  -> *
  where

  Field :: forall (s :: Symbol) cs f a. 
           ( KnownSymbol s
           , AllSatisfied cs a
           ) 
        => !(f a) 
        -> Field s cs f a 

-- | 'showField'
instance (Show1 f) => Show (Field k (Show ': cs) f a) where
  showsPrec = showsPrecedent_Field

showsPrecedent_Field 
  :: (Show1 f) 
  => Showing (Field k (Show ': cs) f a) 
showsPrecedent_Field d f@(Field x) = showParen (d `greaterThan` defaultPrecedence) $
   -- (s "Field @" <> k <> s " " <> v)
   -- NOTE `Semigroup (a -> m)` is NOT `Semigroup (Endo m)`
   (s "Field @" . k . s " " . v)
   where
   k = showString $ show (reifyFieldName f)
   v = showsPrec1 (1+defaultPrecedence) x
   s = showString

showField :: (Show1 f) => Field k (Show ': cs) f a -> String  
showField f@(Field x) = "Field @" <> k <> " (" <> v <> ")"
   where
   k = reifyFieldName f
   v = show1 x

mapRecordFunctors
  :: (forall x. f x -> g x)
  -> Record cs f rs
  -> Record cs g rs
mapRecordFunctors u = \case
  R -> R
  ((Field x) :* xs) -> Field (u x) :* (u `mapRecordFunctors` xs)
{-# INLINE mapRecordFunctors #-}

rmap
  :: (forall x. (c x) => f x -> g x)
  -> Record (c ': cs) f rs
  -> Record (c ': cs) g rs
rmap u = \case
  R -> R
  (Field x :* xs) -> Field (u x) :* (u `rmap` xs)
{-# INLINE rmap #-}

mapRecordFields
  :: (forall x k. Field k cs f x -> Field k ds g x)
  -> Record cs f rs
  -> Record ds g rs
mapRecordFields u = \case
  R -> R
  (Field x :* xs) -> u (Field x) :* (u `mapRecordFields` xs)
{-# INLINE mapRecordFields #-}

-- TODO 
-- infixr 5  <+>
-- infixl 8 <<$>>
-- infixl 8 <<*>>

constrained 
  :: forall (c :: k -> Constraint).
     forall cs f kvs. 
     ( AllTypes c kvs
     )
  => Record       cs  f kvs
  -> Record (c ': cs) f kvs
constrained = reifyConstraint (P @(k -> Constraint) @c)

unconstrained
  :: (cs ~ '[]) 
  => Record cs  f kvs
  -> Record '[] f kvs
unconstrained = mapRecordFields go
  where
  go :: Field k cs f x -> Field k '[] f x
  go (Field x) = Field x

constrain' 
  :: forall (c :: k -> Constraint).
     forall cs cs' f kvs. 
     ( AllTypes c kvs
     , cs  ~ '[] 
     , cs' ~ '[c]
     )
  => Record cs  f kvs
  -> Record cs' f kvs
constrain' = dropConstraints' > reifyConstraint (P @(k -> Constraint) @c)

dropConstraints'
  :: (cs' ~ '[], cs' ~ cs) 
  => Record cs  f kvs
  -> Record cs' f kvs
dropConstraints' = mapRecordFields go
  where
  go :: Field k cs f x -> Field k '[] f x
  go (Field x) = Field x

constrain 
  :: forall (c :: k -> Constraint).
     forall cs f kvs. 
     ( AllTypes c kvs
     )
  => Record cs   f kvs
  -> Record '[c] f kvs
constrain = dropConstraints > reifyConstraint  (P @(k -> Constraint) @c)

dropConstraints
  :: Record cs  f kvs
  -> Record '[] f kvs
dropConstraints = mapRecordFields go
  where
  go :: Field k cs f x -> Field k '[] f x
  go (Field x) = Field x

{- ERROR

    * Could not deduce: (AllSatisfied cs0 Int, AllSatisfied cs0 [Char])
        arising from a use of dog_XOverloadedLabels_Identity'
    * In the second argument of `constrain', namely
    
-}

{-| sugar for a type-level pair.

@
>>> :set 
>>> :set -XDataKinds
>>> type Dog f = Record f ["name" ::: String, "age" ::: Natural]
@

-}
type (:::) k a = '(k, a)
infixr 1 ::: 

{-| constrain each field name in a record with the first constraint @ck@ (frequently, `KnownSymbol`), 
and constrain each field type in that record with the second constraint @cv@. 

for example, 

@
(AllFields KnownSymbol Show \'["enabled" ::: Bool, "setting" ::: String]) => ...
@ 

is equivalent to 

@
(KnownSymbol "enabled", KnownSymbol "setting", Show Bool, Show String) => ... 
@ 

which says that the field names must be type-level strings, 
and that the field types must all be showable.

thus, a generic record function with this constraint can print out the key-value pairs 
of any input record that satisfies those constraints:

@
showRecord :: (AllFields KnownSymbol Show fields) => 'Record' fields -> String 
showRecord RNil      = ...
showRecord (a :& as) = ... 
@

-}
type AllFields cName cType fields = (AllNames cName fields, AllTypes cType fields)

type AllNames c fields = AllConstrained c (GetNames fields)
type AllTypes c fields = AllConstrained c (GetTypes fields)

-- | a type level @fmap fst@
type family GetNames fields where 
  GetNames '[]                    = '[] 
  GetNames ('(k, _v) ': fields) = k ': GetNames fields 

-- | a type level @fmap snd@
type family GetTypes fields where 
  GetTypes '[]                    = '[] 
  GetTypes ('(_k, v) ': fields) = v ': GetTypes fields 

type family AllConstrained c ts :: Constraint where
  AllConstrained c '[] = ()
  AllConstrained c (t ': ts) = (c t, AllConstrained c ts) -- '

type family AllSatisfied cs t :: Constraint where
    AllSatisfied '[]       t = ()
    AllSatisfied (c ': cs) t = (c t, AllSatisfied cs t) -- '

-- | a type level @fmap (fmap fst)@
type family GetInputs fields where 
  GetInputs '[]                        = '[] 
  GetInputs ('(k, '(a, _b)) ': fields) = '(k, a) ': GetInputs fields  -- '

-- | a type level @fmap (fmap snd)@
type family GetOutputs fields where 
  GetOutputs '[]                        = '[] 
  GetOutputs ('(k, '(_a, b)) ': fields) = '(k, b) ': GetOutputs fields  -- '

{- | e.g. @(abs ~ ZipTypes as bs) => ...@. the key is must match. and in particular, the lengths must match.

for example, given @{"a":_, "b":_}@: 

>>> :kind! ZipTypes '["a" ::: Integer, "b" ::: Bool] '["a" ::: String, "b" ::: Char]
  :: [(Symbol, (*, *))]
  = '['("a", '(Integer, [Char])), '("b", '(Bool, Char))]

stays opaque when the keys don't match. given @{"a":_, "b":_}@ and @{"x":_, "b":_}@:

>>> :kind! ZipTypes '["a" ::: Integer, "b" ::: Bool] '["x" ::: String, "b" ::: Char]
  :: [(Symbol, (*, *))]
  = ZipTypes '["a" ::: Integer, "b" ::: Bool] '["x" ::: String, "b" ::: String]

partially evaluates, given @{"a":_, "b":_}@ and @{"a":_, "y":_}@:

>>> :kind! ZipTypes '["a" ::: Integer, "b" ::: Bool] '["a" ::: String, "y" ::: Char]
  :: [(Symbol, (*, *))]
  = '("a", '(Integer, [Char])) ': ZipTypes '["b" ::: Bool] '["y" ::: Char]


-} 
type family ZipTypes as bs where 
  ZipTypes '[]            '[]            = '[] 
  ZipTypes ('(s,a) ': as) ('(s,b) ': bs) = '(s, '(a,b)) ': ZipTypes as bs -- '

-- type Example_ZipTypes = ZipTypes ['("a",Integer),'("b",Bool)] ['("c",String),'("b",String)]  

{-|

-}

--------------------------------------------------------------------------------

class RecordApplicative kvs where
  rPure
    :: forall f. (forall x. f x)
    -> Record_ f kvs

instance RecordApplicative '[] where
  rPure _ = R
  {-# INLINE rPure #-}

instance ( KnownSymbol k
         , RecordApplicative kvs
         ) 
       => RecordApplicative ('(k,v) ': kvs) 
  where
  rPure s = field_ s :* rPure s
  {-# INLINE rPure #-}

proxyRecord 
  :: (RecordApplicative fields) 
  => PRecord_ fields 
proxyRecord = rPure Proxy 

proxyRecordOf  
  :: forall fields. 
     forall proxy. 
     (RecordApplicative fields) 
  => proxy fields 
  -> PRecord_ fields 
proxyRecordOf _ = proxyRecord @fields 

{-| a record where each field is constructed 
from the methods of some class.

-}
methodRecord' 
  :: forall (c :: k -> Constraint) fields f.  
     ( AllTypes c fields
     , RecordApplicative fields
     )
  => (forall x. Dictionary (c x) -> f x)
  -> Record_ f fields 
methodRecord' u 
  = mapRecordFields go (constrainedRecord @k @c @fields) 
  where
  go :: forall s x. PField s '[c] x -> Field s '[] f x 
  go f@(Field Proxy) = Field (u (fieldToFirstDictionary f)) 

constrainedRecord 
  :: forall (constraint :: k -> Constraint) (fields :: [(Symbol,k)]). 
     ( RecordApplicative fields
     , AllTypes constraint fields 
     ) 
  => PRecord '[constraint] fields 
constrainedRecord = constrain @k @constraint (rPure (Proxy)) 

constrainedRecordOf  
  :: forall (constraint :: k -> Constraint). 
     forall fields proxy. 
     ( RecordApplicative fields
     , AllTypes constraint fields 
     ) 
  => proxy constraint 
  -> PRecord '[constraint] fields 
constrainedRecordOf _ = constrainedRecord @k @constraint 

rTraverse
  :: forall h f g cs rs. 
     ( Applicative h
     ) 
  => (forall x. String -> f x -> h (g x))
  ->    Record cs f rs
  -> h (Record cs g rs)
rTraverse u = go
  where
  go
    :: forall ss.
       ( Applicative h
       ) 
    =>    Record cs f ss
    -> h (Record cs g ss)
  go = \case 
      R                   -> pure R 
      (f@(Field x) :* xs) -> (:*) 
           <$> (let k = reifyFieldName f in Field <$> u k x) 
           <*> go xs
{-# INLINABLE rTraverse #-}

fieldToSymbolDictionary :: Field k cs f a -> Dictionary (KnownSymbol k) 
fieldToSymbolDictionary Field{} = Dict 

symbolValue_Dictionary :: forall k. Dictionary (KnownSymbol k) -> String 
symbolValue_Dictionary Dict = symbolVal (P @Symbol @k)

fieldToFirstDictionary :: Field k (c ': cs) f a -> Dictionary (c a)   
fieldToFirstDictionary Field{} = Dict

--------------------------------------------------------------------------------

-- the empty symbol is not a valid key
-- injectGeneralRecord :: V.Rec f as -> Record f (FMAP ("",) as)

reifyConstraint
  :: forall (c :: k -> Constraint) cs (f :: k -> *) kvs proxy. 
     ( AllTypes c kvs 
     )
  => proxy c
  -> Record cs        f kvs
  -> Record (c ': cs) f kvs
reifyConstraint proxy = \case
  R -> R
  (Field x :* xs) -> (Field x) :* reifyConstraint proxy xs

displayRecord 
  :: forall f fields. (Show1 f) 
  => Record '[Show] f fields -> String 
displayRecord r = "{ " <> intercalate ", " (go r) <> " }"
    where
    go :: forall fields'. Record '[Show] f fields' -> [String] 
    go R         = []
    go (f :* fs) = displayField f : go fs 

displayField :: (Show1 f) => Field k '[Show] f a -> String 
displayField f@(Field x) = k <> ": " <> v
   where
   k = reifyFieldName f
   v = show1 x

displayIdentityRecord 
  :: forall fields.
     Record '[Show] Identity fields -> String 
displayIdentityRecord r = "{ " <> intercalate ", " (go r) <> " }"
    where
    go :: forall fields'. Record '[Show] Identity fields' -> [String] 
    go R         = []
    go (f :* fs) = displayIdentityField f : go fs 

displayIdentityField :: Field k '[Show] Identity a -> String 
displayIdentityField f@(Field (Identity x)) = k <> " = " <> v
   where
   k = reifyFieldName f
   v = show x

{-| reifies the key of a field. 

i.e. the type-level string @k@ becomes a (value-level) string. 

-}
reifyFieldName :: forall k cs f a. Field k cs f a -> String 
reifyFieldName Field{} = symbolVal (Proxy :: Proxy k)

(***) :: (KnownSymbol k) 
     => Field k '[] f a
     -> Record '[] f            fields
     -> Record '[] f ('(k,a) ': fields)
(***) = (:*)

{-ERROR

    * Could not deduce: (AllSatisfied cs0 Int, AllSatisfied cs0 [Char])
        arising from a use of `dog_XOverloadedLabels_Identity'
    * In the first argument of `dropConstraints', namely
        `dog_XOverloadedLabels_Identity'

(***) :: (KnownSymbol k, AllSatisfied cs a) 
     => Field k cs f a
     -> Record cs f            fields
     -> Record cs f ('(k,a) ': fields)
(***) = (:*)
-}

{-| uses `-XTypeApplications`.

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = field @"name" "loki" :# field @"age" 7 :# R
@

-}
field
  :: forall (k :: Symbol) a. forall cs f. 
  (KnownSymbol k, AllSatisfied cs a) 
  => f a -> (Field k) cs f a
field = Field

{-NOTE

    Unused quantified type variable `(k :: Symbol)'
    In the type `forall (k :: Symbol) a. a -> a'
   |

-}

{-| uses `-XTypeApplications`.

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = ifield @"name" "loki" :* ifield @"age" 7 :* R
@

-}
ifield 
  :: forall (k :: Symbol) a. forall cs. 
  (KnownSymbol k, AllSatisfied cs a) 
  => a -> (Field k) cs Identity a
ifield = Identity > field

{-| uses `-XTypeApplications`.

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = cfield @"name" "loki" :* cfield @"age" @Int (show 7) :* R
@

-}
cfield 
  :: forall (k :: Symbol) b. forall a cs. 
  (KnownSymbol k, AllSatisfied cs b) 
  => a -> (Field k) cs (Const a) b -- cs='[]
cfield = Const > field

{-| uses `-XTypeApplications`.

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = mfield @"name" "loki" :* mfield @"age" @Int 7 :* R
@

-}
mfield
  :: forall (k :: Symbol) a. forall f cs. 
  (KnownSymbol k, AllSatisfied cs a, Applicative f) 
  => a -> (Field k) cs f a
mfield = pure > field

-------------------------------------------------------------------------------

{-| uses `-XOverloadedLabels`.

@
>>> :set -XOverloadedLabels
>>> :set -XDataKinds
>>> dog = #name-: "loki" :* #age-: 7 :* R
@

-}
(-:) 
  :: (AllSatisfied cs a) 
  => Label k -> f a -> Field k cs f a
Label -: x = Field x
-- NOTE the proxy must be concrete for the inference of IsLabel

(=:)
  :: (AllSatisfied cs a) 
  => Label k -> a -> Field k cs Identity a
Label =: x = Field (Identity x)

{-ERROR

C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Example.hs:31:35-55: error:
    * Ambiguous type variable `k0' arising from a use of `dog_XOverloadedLabels'
      prevents the constraint `(GHC.OverloadedLabels.IsLabel
                                  "age" (Label k0))' from being solved.
      Probable fix: use a type annotation to specify what `k0' should be.
      These potential instance exist:
        instance GHC.TypeLits.KnownSymbol k =>
                 GHC.OverloadedLabels.IsLabel k (Label k)


KnownSymbol k, IsLabel k (Label k), 

-}

-- (=:) :: forall k a. () => Label k -> a -> a
-- Label =: x = x

-- | like 'Proxy' for 'Symbols'. 
data Label k where 
  Label :: forall k. (KnownSymbol k) => Label k

label :: forall k. forall proxy. (KnownSymbol k) => proxy k -> Label k
label _ = Label @k

instance forall k k'. (KnownSymbol k, k ~ k') => IsLabel k (Label k') where
    fromLabel = Label @k -- label (Proxy @k)

{- NOTE needs the constraint trick for inference

*Vinyl.Fields.Example> :t dog_XOverloadedLabels
dog_XOverloadedLabels
  :: (GHC.TypeLits.KnownSymbol k1, GHC.TypeLits.KnownSymbol k2,
      GHC.OverloadedLabels.IsLabel "name" (Label k1),
      GHC.OverloadedLabels.IsLabel "age" (Label k2),
      AllSatisfied cs [Char], AllSatisfied cs Int) =>
     Record
       cs Data.Functor.Identity.Identity '['(k1, [Char]), '(k2, Int)]


-}

{-
Orphan instance: instance IsLabel k (Proxy k)
    To avoid this
        move the instance declaration to the module of the class or of the type, or
        wrap the type with a newtype and declare the instance on the new type.
    |
-}

