{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-} 
{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

{-| 

-}
module Vinyl.Fields.Types where
-- import Vinyl.Fields.Extra
import Prelude.Spiros 

-- import Control.Applicative
-- import Control.Monad

-- import Data.Vinyl hiding (ElField(..))
-- import Data.Vinyl.TypeLevel 
-- -- import Data.Aeson
-- -- import Data.Aeson.Types
-- import qualified Data.Text.Lazy as T
import Data.Functor.Identity

import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Kind 
import Data.Functor.Compose 

--------------------------------------------------------------------------------

{-| 

`fields` should be a type-level set (i.e. `Map Symbol *`), 
since we don't want duplicate field names, and since we don't care about order,
but GHC currently only has type-level lists. 

@
>>> :set -XDataKinds
>>> type Dog f = Record f ['("name",String), '("age",Natural)]
>>> dog = (Identity "loki" :* Identity 7 :* R) :: Dog Identity
@

-}
data Record :: (* -> *) 
            -> [(Symbol,*)]
            -> *
    where

    R      :: forall f. Record f '[]

    (:*)   :: (Field k f a) 
           -> (Record f            fields) 
           -> (Record f ('(k,a) ': fields))

{-| like @f@ composed with a @KnownSymbol@ dictionary. 
almost an @Identity@ functor.

-}
data Field k f a where
     Field :: forall k f a. (KnownSymbol k) => !(f a) -> Field k f a 

infixr 7 :*
infix  8 -:
-- infixr 5  <+>
-- infixl 8 <<$>>
-- infixl 8 <<*>>

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

data Constrained c a where
    Constrained :: forall c a. 
                ( c a
                ) 
                => !a 
                -> Constrained c a 

data Constrained' c k f a where
    Constrained' :: forall c k f a. 
                ( c a
                , KnownSymbol k
                ) 
                => !(f a) 
                -> Constrained' c k f a 

--------------------------------------------------------------------------------

-- the empty symbol is not a valid key
-- injectGeneralRecord :: V.Rec f as -> Record f (FMAP ("",) as)

reifyConstraint
  :: AllTypes c kvs 
  => proxy c
  -> Record f kvs
  -> Record (Compose (Constrained c) f) kvs
reifyConstraint proxy = \case
  R -> R
  (x :* xs) -> Compose (Constrained x) :* reifyConstraint proxy xs

displayRecord :: Record f fields -> String 
displayRecord r = "{ " <> go r <> " }"
    where
    go :: Record f' fields' -> String 
    go R         = ""
    go (f :* fs) = displayField f <> ", " <> go fs 

displayField :: (Show (f a)) => Field k f a -> String 
displayField f@(Field x) = k <> ": " <> v
   where
   k = reifyFieldName f
   v = show x
 
reifyFieldName :: forall k f a. Field k f a -> String 
reifyFieldName Field{} = symbolVal (Proxy :: Proxy k)

(**) :: forall k a fields. (KnownSymbol k) 
     => a 
     -> Record Identity            fields
     -> Record Identity ('(k,a) ': fields)
x ** xs = Field (Identity x) :* xs

{-| uses `-XTypeApplications`.

TODO AllowAmbiguousTypes

@
>>> :set -XTypeApplications
>>> :set -XDataKinds
>>> dog = field @"name" "loki" :# field @"age" 7 :# R
@

-}
field :: forall (k :: Symbol) f a. (KnownSymbol k) => f a -> (Field k) f a
field = Field

{-NOTE

    Unused quantified type variable `(k :: Symbol)'
    In the type `forall (k :: Symbol) a. a -> a'
   |

-}

-- {-| uses `-XTypeApplications`.

-- TODO AllowAmbiguousTypes

-- @
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> dog = field @"name" "loki" :* field @"age" 7 :* R
-- @

-- -}
-- ifield :: forall k a. (KnownSymbol k) => a -> Identity a
-- ifield = Identity 

-- {-| uses `-XTypeApplications`.

-- TODO AllowAmbiguousTypes

-- @
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> dog = field @"name" "loki" :* field @"age" 7 :* R
-- @

-- -}
-- cfield :: forall k a b. (KnownSymbol k) => b -> Const b a 
-- cfield = Const

-- {-| uses `-XTypeApplications`.

-- TODO AllowAmbiguousTypes

-- @
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> dog = field @"name" "loki" :* field @"age" 7 :* R
-- @

-- -}
-- mfield :: forall k a f. (KnownSymbol k, Applicative f) => a -> f a
-- mfield = pure

{-| uses `-XOverloadedLabels`.

@
>>> :set -XOverloadedLabels
>>> :set -XDataKinds
>>> dog = #name-: "loki" :* #age-: 7 :* R
@

-}
(-:) :: forall k a. (KnownSymbol k) => Label k -> a -> a
Label -: x = x
-- NOTE the proxy must be concrete for the inference of IsLabel

-- | like 'Proxy' for 'Symbols'. 
data Label k where Label :: (KnownSymbol k) => Label k

instance (KnownSymbol k) => IsLabel k (Label k) where
    fromLabel = Label

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