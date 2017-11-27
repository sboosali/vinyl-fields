{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, TypeOperators, TypeApplications #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-} 
{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

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
import Data.Functor.Classes

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
data Record :: [* -> Constraint] 
            -> (* -> *) 
            -> [(Symbol,*)]
            -> *
    where

    R      :: Record cs f '[]

    (:*)   :: (Field k cs f a) 
           -> (Record cs f            fields) 
           -> (Record cs f ('(k,a) ': fields))

-- -- instance (Show `In` cs) => Show (Record cs f as) where
-- instance (AllTypes Show as) => Show (Record cs f as) where
--   show = showRecord
instance (AllTypes Show kvs, Show1 f) => Show (Record cs f kvs) where
  show = showRecord

showRecord 
  :: (AllTypes Show as, Show1 f) 
  => (Record cs f as) -> String
showRecord r = showShowableRecord (reifyConstraint cShow r)
   where
   cShow = P @Show  -- P @(Type -> Constraint) @Show

showShowableRecord 
  :: forall f cs as. (Show1 f) 
  => Record (Show ': cs) f as 
  -> String
showShowableRecord 
  = mapRecord _show
  > recordToList > (<> ["R"])
  > intercalate " :* "
  where
  _show :: forall k. forall a.
           Field k (Show ': cs) f          a 
--      -> Field k (Show ': cs) (C String) a
        -> Field k cs           (C String) a
  _show f@(Field{}) = f & showField > cfield @k

-- showRecord 
--   :: (AllTypes Show as, Show1 f) 
--   => (Record cs f as) -> String
-- showRecord r = showShowableRecord (reifyConstraint cShow r)
--    where
--    cShow = P @Show -- P @(Type -> Constraint) @Show
-- -- the polykinded Proxy needs the kind first!

-- showShowableRecord 
--   :: (Show1 f) 
--   => Record (Show ': cs) f as 
--   -> String
-- showShowableRecord = rmap _show > recordToList > intercalate " :* " --  > rmap f2c
--   where
--   _show x = C $ show1 x

-- | A record with uniform fields may be turned into a list.
recordToList
  :: Record cs (Const a) bs
  -> [a]
recordToList = \case
  R               -> []
  (Field x :* xs) -> getConst x : recordToList xs

{-| like @f@ composed with a @KnownSymbol@ dictionary. 
almost an @Identity@ functor.

-}
data Field k cs f a where
  Field :: forall k cs f a. 
           ( KnownSymbol k
           , AllSatisfied cs a
           ) 
        => !(f a) 
        -> Field k cs f a 

-- | 'showField'
instance (Show1 f) => Show (Field k (Show ': cs) f a) where
  show = showField 

showField :: (Show1 f) => Field k (Show ': cs) f a -> String  
showField f@(Field x) = "Field @" <> k <> " (" <> v <> ")"
   where
   k = reifyFieldName f
   v = show1 x

-- instance (Show (f a)) => Show (Field k cs f a) where
--   show = showField 

-- showField :: (Show (f a)) => Field k cs f a -> String 
-- showField f@(Field x) = "Field @" <> k <> " (" <> v <> ")"
--    where
--    k = reifyFieldName f
--    v = show x
-- -- Field k (Show ': cs) f a -> String 

infixr 7 :*
infixr 1 ***

infix  2 -:
infix  2 =:

rmap
  :: (forall x. (c x) => f x -> g x)
  -> Record (c ': cs) f rs
  -> Record (c ': cs) g rs
rmap η = \case
  R -> R
  (Field x :* xs) -> Field (η x) :* (η `rmap` xs)
{-# INLINE rmap #-}

mapRecord
  :: (forall x k. Field k cs f x -> Field k ds g x)
  -> Record cs f rs
  -> Record ds g rs
mapRecord η = \case
  R -> R
  (Field x :* xs) -> η (Field x) :* (η `mapRecord` xs)
{-# INLINE mapRecord #-}
-- :: (forall x k. Field k (c ': cs) f x -> Field k (c ': cs) g x)

-- infixr 5  <+>
-- infixl 8 <<$>>
-- infixl 8 <<*>>

constrain 
  :: forall (c :: * -> Constraint).
     forall cs f kvs. 
     ( AllTypes c kvs
     )
  => Record cs   f kvs
  -> Record '[c] f kvs
constrain = dropConstraints > reifyConstraint (P @c)

dropConstraints
  :: Record cs  f kvs
  -> Record '[] f kvs
dropConstraints = mapRecord go
  where
  go :: Field k cs f x -> Field k '[] f x
  go (Field x) = Field x

-- constrain = reifyConstraint (P @c) > loosenConstraints

-- loosenConstraints
--   :: (ds `Subset` cs) 
--   => Record cs f kvs
--   -> Record ds f kvs
-- loosenConstraints

-- constrain 
--   :: forall (c :: * -> Constraint).
--      forall cs f kvs. 
--      ( AllTypes c kvs
--      )
--   => Record (     cs) f kvs
--   -> Record (c ': cs) f kvs
-- constrain = reifyConstraint (P @c)

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
  AllConstrained c (t ': ts) = (c t, AllConstrained c ts)

-- data Dictionary c a where
--   Dictionary :: c a => a -> Dict c a
-- data Dictionary0 c a where
--     Dictionary0 :: c a => a -> Dict c a
-- data Dictionary1 c a where
--   Dictionary1 :: c a => a -> Dict c a

type family AllSatisfied cs t :: Constraint where
    AllSatisfied '[]       t = ()
    AllSatisfied (c ': cs) t = (c t, AllSatisfied cs t)

-- data Constrained c a where
--     Constrained :: forall c a. 
--                 ( c a
--                 ) 
--                 => !a 
--                 -> Constrained c a 

-- data Constrained' c k f a where
--     Constrained' :: forall c k f a. 
--                 ( c a
--                 , KnownSymbol k
--                 ) 
--                 => !(f a) 
--                 -> Constrained' c k f a 

--------------------------------------------------------------------------------

-- the empty symbol is not a valid key
-- injectGeneralRecord :: V.Rec f as -> Record f (FMAP ("",) as)

reifyConstraint
  :: forall (c :: * -> Constraint) cs f kvs proxy. 
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

-- (***) :: (KnownSymbol k, AllSatisfied cs a) 
--      => Field k cs f a
--      -> Record cs f            fields
--      -> Record cs f ('(k,a) ': fields)
-- (***) = (:*)
-}

-- (***) :: forall k cs a fields. 
--      (KnownSymbol k, AllSatisfied cs a) 
--      => a 
--      -> Record cs Identity            fields
--      -> Record cs Identity ('(k,a) ': fields)
-- x *** xs = Field (Identity x) :* xs

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

-------------------------------------------------------------------------------

{-
Orphan instance: instance IsLabel k (Proxy k)
    To avoid this
        move the instance declaration to the module of the class or of the type, or
        wrap the type with a newtype and declare the instance on the new type.
    |
-}

{-

{-| 

@ 
'Field' :: ("" ':::' )
@ 

NOTE if you mistype (k :: v) i.e. a type/kind signature, 
instead of (k ::: v) i.e. an application of this type constructor, 
you may get this error message: 

@
    Variable `v' used as both a kind and a type
    Did you intend to use TypeInType?
@

-}
data (:::) (k :: Symbol) (v :: *) :: * where
  Field :: KnownSymbol k => !v -> (k ::: v) 
deriving instance Functor ((:::) k)

type Field = (:::)

{-| note 

if you mistype (k :: v) i.e. a type/kind signature, instead of (k ::: v) i.e. the applied type constructor, 
you may get this error message: 

    Variable `v' used as both a kind and a type
    Did you intend to use TypeInType?

-}

reifyFieldName :: forall k v. (k ::: v) -> String 
reifyFieldName Field{} = symbolVal (Proxy :: Proxy k)

-- reifyFieldName :: forall k v. (k ::: v) -> String 
-- reifyFieldName Field{} = symbolVal (Proxy :: Proxy k)

{-| 

@
GetName ("" ':::' )  ~  "" 
@

-}
type family GetName field :: Symbol where 
  GetName (s ::: _a) = s 

{-| 

@
GetType ("" ':::' )  ~  
@ 

-}
type family GetType field :: * where 
  GetType (_s ::: a) = a 

{-| 

@ 
field @"" 
@

-}
field :: forall s a. KnownSymbol s => a -> Field s a
field = Field 

{-| 

-}
type Record = Rec Identity -- Field 

-- type ProxyRecord = Rec ProxyField 

type ProxyField s a = Field s (Proxy a)
-- type ProxyField s a = Proxy (Field s a) 

{-| if we can parse each field in a record, then we can parse the record itself. 

-}
type RecordParser = Rec FieldParser

{-| like 'ElField' `Compose`d with a @`Dict` `KnownSymbol`@. 

-}
data FieldParser a 
  = FieldParser Text (JSONParser a)
  deriving (Functor)

-- data FieldParser s a 
--   = FieldParser Text (JSONParser (s ::: a))
--   deriving (Functor)

{-| 

-}
type JSONParser a = (Value -> Parser a) 

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
  GetNames ((k ::: _v) ': fields) = k ': GetNames fields 

-- | a type level @fmap snd@
type family GetTypes fields where 
  GetTypes '[]                    = '[] 
  GetTypes ((_k ::: v) ': fields) = v ': GetTypes fields 

type family PairFromField fields :: (Symbol, *) where 
  PairFromField (k ::: v) = '(k, v)
  
type family PairsFromFields fields :: [(Symbol, *)] where 
  PairsFromFields '[]               = '[] 
  PairsFromFields (field ': fields) = PairFromField field ': PairsFromFields fields  

type List = ([]) 

z :: Record '[] 
z = RNil 


{-| 

Build a record whose fields are derived solely from a
pair of constraints satisfied by each key-value pair.

-}

--------------------------------------------------------------------------------

defaultRecordParser 
  :: 
     ( AllFields KnownSymbol FromJSON fields 
     ) 
  => RecordParser fields 
defaultRecordParser = todo 

parseJSONField 
  :: ( FromJSON    v
     ) 
  =>             (k :::  Proxy v) -- ProxyField k v
  -> FieldParser (k :::        v) -- FieldParser (Field k v)    
parseJSONField kv@Field{} = FieldParser keyName keyParser
  where 
  keyName   = reifyFieldName kv & T.pack 
  keyParser = fmap Field <$> parseJSON -- genericParseJSON defaultOptions 

defaultFieldParser
  :: forall k v. 
     ( FromJSON    v
     , KnownSymbol k
     ) 
  => FieldParser (k ::: v) 
defaultFieldParser = FieldParser keyName valueParser
  where 
  keyName     = symbolVal (Proxy :: Proxy k) & T.pack 
  valueParser = fmap Field <$> parseJSON

-- fromFieldParser :: FieldParser k v -> Parser (Field k v)
-- fromFieldParser = todo 

-- {-| 

-- similar to:

-- @
-- :: Rec (JSONParser ':.' 'Field') kvs -> JSONParser (Rec Field kvs)
-- @

-- which has the shape of a 'rtraverse':

-- @
-- :: (Applicative m, Functor f) => Rec (m :. f) kvs -> m (Rec f kvs)
-- @

-- -}
-- objectParseJSON
--   :: Rec JSONFieldParser kvs
--   -> JSONParser (Rec Field kvs)
-- objectParseJSON RNil                           _ = pure RNil
-- objectParseJSON (kv@(JSONFieldParser p) :& ps) v = 
--       (:&) 
--   <$> (Field <$> withObject "<record>" (\o -> explicitParseField p o k) v)  
--   <*> objectParseJSON ps v 
--   where 
--   k = reifyFieldName kv & T.pack 

-- maybe "<record>" id $ mName 

--------------------------------------------------------------------------------

{-


data Record :: (functor :: Symbol -> Type -> Type) 
               (fields  :: [(Symbol,Type)])
            -> * where

    RNil :: Record f '[]
    (:*) :: !(f k a) -> !(Record f fields) -> Record f ('(k,a) ': fields)


    data Record :: (functor :: Symbol -> Type -> Type) 
               (fields  :: [(Symbol,Type)])
            -> Type
    where

    Record :: Record f '[]

    (:*)   :: !(f k a) 
           -> !(Record f fields) 
           -> Record f ('(k,a) ': fields)


-------------------------------------------------------------------------------

the unquantified variable is ignored.

> :t dog_TypeApplications

error:
    * No instance for (Show
                         (Record
                            Data.Functor.Identity.Identity
'['(k10, [Char]), '(k20, Int)]))
        arising from a use of `print'
    * In a stmt of an interactive GHCi command: print it
*Vinyl.Fields.Example> :t dog_TypeApplications
dog_TypeApplications
  :: (GHC.TypeLits.KnownSymbol k1, GHC.TypeLits.KnownSymbol k2) =>
     Record Data.Functor.Identity.Identity '['(k1, [Char]), '(k2, Int)]

-------------------------------------------------------------------------------

{-| uses `-XTypeApplications`.

TODO AllowAmbiguousTypes

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = field @"name" "loki" :* field @"age" 7 :* R
@

-}
field :: forall k a. (KnownSymbol k) => a -> a
field = id

ERROR

C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Types.hs:85:10-46: error:
    * Could not deduce (KnownSymbol k0)
      from the context: KnownSymbol k
        bound by the type signature for:
                   field :: forall (k :: Symbol) a. KnownSymbol k => a -> a
        at C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Types.hs:85:10-46
      The type variable `k0' is ambiguous
    * In the ambiguity check for `field'
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      In the type signature:
        field :: forall k a. (KnownSymbol k) => a -> a
   |


-------------------------------------------------------------------------------

(-:) :: forall k a. (KnownSymbol k) => Label k -> a -> a
(-:) _ = id
-- NOTE the proxy must be concrete for the inference of IsLabel

-- | like 'Proxy' for 'Symbols'. 
data Label k where Label :: (KnownSymbol k) => Label k

instance (KnownSymbol k) => IsLabel k (Label k) where
    fromLabel = Label
          
ERROR:

C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Example.hs:34:25-29: error:
    * Could not deduce (GHC.OverloadedLabels.IsLabel "name" (Label k0))
        arising from the overloaded label `#name'
      from the context: GHC.TypeLits.KnownSymbol k
        bound by the inferred type of
                 dog_XOverloadedLabels :: GHC.TypeLits.KnownSymbol k =>
                                          Record [] '['(k, Char)]
        at C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Example.hs:34:1-44
      The type variable `k0' is ambiguous
      These potential instance exist:
        instance GHC.TypeLits.KnownSymbol k =>
                 GHC.OverloadedLabels.IsLabel k (Label k)
          -- Defined at C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Types.hs:145:10
    * In the first argument of `(-:)', namely `#name'
      In the first argument of `(:*)', namely `#name -: "loki"'
      In the expression: #name -: "loki" :* R
   |
C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Example.hs:34:25-39: error:
    * Could not deduce (GHC.TypeLits.KnownSymbol k0)
        arising from a use of `-:'
      from the context: GHC.TypeLits.KnownSymbol k
        bound by the inferred type of
                 dog_XOverloadedLabels :: GHC.TypeLits.KnownSymbol k =>
                                          Record [] '['(k, Char)]
        at C:\Users\Spiros\haskell\vinyl-fields\sources\Vinyl\Fields\Example.hs:34:1-44
      The type variable `k0' is ambiguous
    * In the first argument of `(:*)', namely `#name -: "loki"'
      In the expression: #name -: "loki" :* R
      In an equation for `dog_XOverloadedLabels':
          dog_XOverloadedLabels = #name -: "loki" :* R


-} -}