{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns  #-}
{-# LANGUAGE KindSignatures, TypeOperators, TypeApplications #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-} 
{-# LANGUAGE ScopedTypeVariables, DataKinds, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- we're instantiating FromJSON

{-| 

-}
module Vinyl.Fields.Json where
import Vinyl.Fields.Extra
import Vinyl.Fields

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.Text as T
-- import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString) 
import Data.Functor.Identity
import Data.Constraint 

-- import GHC.TypeLits

type JasonParser' a = J.Value -> J.Parser a

data JasonParser a = JasonParser { getJasonParser :: JasonParser' a} 

{- OLD 

newtype ... 
  deriving (Functor,Applicative)

ERROR 

  * Can't make a derived instance of `Applicative JasonParser'
      (even with cunning GeneralizedNewtypeDeriving):
      cannot eta-reduce the representation type enough
  * In the newtype declaration for `JasonParser'

-} 

--------------------------------------------------------------------------------

{-

Decoding without FromJSON instances: 

eitherDecodeWith :: Parser Value -> (Value -> IResult a) -> ByteString -> Either (JSONPath, String)

data IResult a
| IError JSONPath String	 
| ISuccess a

type JSONPath = [JSONPathElement]

data JSONPathElement
| Key Text	
JSON path element of a key into an object, "object.key".
| Index !Int	
JSON path element of an index into an array, "array[index]".


-}

decodeRecord 
  :: forall fields. 
    ( AllTypes FromJSON fields 
    , RecordApplicative fields
    ) 
    -- => Maybe String -- TODO Name 
    => ByteString 
    -> Either String (R fields)
decodeRecord s = 
-- decodeRecord objectName s = 
    J.eitherDecode' s -- (parseJSON_Record @fields)

-- type JasonRecord cs kvs = Record (FromJSON ': cs) Identity kvs

-- | 'parseJSON_Record' 
instance forall fields.   
  ( AllTypes FromJSON fields 
  , RecordApplicative fields
  ) 
  => FromJSON (R fields) where
    parseJSON = parseJSON_Record @fields 

parseJSON_Record 
  :: forall fields. 
  ( AllTypes FromJSON fields 
  , RecordApplicative fields
  ) 
  => JasonParser' (IRecord_ fields) 
parseJSON_Record = parseJSON_Record' @fields Nothing & getJasonParser 

parseJSON_Record' 
  :: forall fields. 
     ( AllTypes FromJSON fields 
     , RecordApplicative fields
     ) 
  => Maybe String -- TODO Name 
  -> JasonParser (IRecord_ fields) 
parseJSON_Record' objectName = 
    objectParserFromFieldParsers objectName (defaultRecordParser @fields)

defaultRecordParser 
  :: forall fields. 
     ( AllTypes FromJSON fields 
     , RecordApplicative fields
     ) 
  => Record_ JasonParser fields 
defaultRecordParser = methodRecord' go
    where
    go :: forall x. Dictionary (FromJSON x) -> JasonParser x
    go Dict = defaultJasonParser

objectParserFromFieldParsers 
  :: Maybe String 
  ->              Record_ JasonParser fields 
  -> JasonParser (Record_ Identity    fields)
objectParserFromFieldParsers objectName rs = 
  JasonParser $ \v -> rTraverse (go v) rs
  where 
  n = maybe "<record>" id objectName 
  -- go :: forall x. JasonParser x -> JasonParser' (Identity x)
  go :: forall x. J.Value -> (String -> JasonParser x -> J.Parser (Identity x))
  go v (T.pack -> k) (JasonParser p) = 
       Identity <$> (v & withObject n (\o -> 
           explicitParseField p o k))

defaultJasonParser 
  :: ( FromJSON a ) 
  => JasonParser a  
defaultJasonParser = JasonParser parseJSON 

-- defaultFieldParser 
--   ::  
--      ( FromJSON a  
--      ) 
--   => proxy a 
--   -> Field_ k JasonParser a  
-- defaultFieldParser _ = Field (JasonParser parseJSON) 

-- objectParseJSON
--   ::              RecordOf FromJSON Proxy    kvs
--   -> JasonParser (Record_           Identity kvs)
-- objectParseJSON = \case 
--   R -> const $ pure R
--   (f@(Field p) :* ps) -> \v -> (:*) 
--     <$> (Field <$> withObject "<record>" (\o -> explicitParseField p o k) v)  
--     <*> objectParseJSON ps v 
--     where 
--     k = reifyFieldName kv & T.pack 

dictionaries_FromJSON 
  :: forall fields. 
     ( RecordApplicative fields
     , AllTypes FromJSON fields 
     ) 
  => PRecord '[FromJSON] fields 
dictionaries_FromJSON = constrainedRecord 

{- OLD 

objectParserFromFieldParsers 
  ::              Record_ JasonParser kvs
  -> JasonParser (Record_ Identity    kvs)
objectParserFromFieldParsers = JasonParser go
  where 
  go = \case 
      R                   -> \_ -> pure R
      (f@(Field p) :* ps) -> \v -> let k = reifyFieldName kv & T.pack in (:*) 
        <$> (Field <$> withObject "<record>" (\o -> explicitParseField p o k) v)  
        <*> go ps v

  ::              RecordOf FromJSON f kvs
  -> JasonParser (Record_           f kvs) 
   
-}

--------------------------------------------------------------------------------

-- type JasonField k cs kvs = Field k (FromJSON ': cs) Identity kvs

-- -- | 'parseJSON_Field' 
-- instance (FromJSON (f a)) => FromJSON (Field k cs f a) where
--     parseJSON = parseJSON_Field 

-- parseJSON_Field 
--   :: (FromJSON (f a)) 
--   => JasonParser (Field k cs f a)
-- parseJSON_Field = todo 

-- parseJSONField 
--   :: ( FromJSON    v
--      ) 
--   =>             (k :::  Proxy v) -- ProxyField k v
--   -> FieldParser (k :::        v) -- FieldParser (Field k v)    
-- parseJSONField kv@Field{} = FieldParser keyName keyParser
--   where 
--   keyName   = reifyFieldName kv & T.pack 
--   keyParser = fmap Field <$> parseJSON -- genericParseJSON defaultOptions 

-- defaultFieldParser
--   :: forall k v. 
--      ( FromJSON    v
--      , KnownSymbol k
--      ) 
--   => FieldParser (k ::: v) 
-- defaultFieldParser = FieldParser keyName valueParser
--   where 
--   keyName     = symbolVal (Proxy :: Proxy k) & T.pack 
--   valueParser = fmap Field <$> parseJSON

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

{- OLD 

type JasonRecord cs kvs = Record (FromJSON ': cs) Identity kvs

-- (AllTypes FromJSON kvs) => 
-- | 'parseJSON_Record' 
instance FromJSON (JasonRecord cs kvs) where
    parseJSON = parseJSON_Record

parseJSON_Record :: JasonParser (JasonRecord cs kvs) 
parseJSON_Record = todo 

type JasonField k cs kvs = Field k (FromJSON ': cs) Identity kvs

-- | 'parseJSON_Field' 
instance FromJSON (JasonField k cs kvs) where
    parseJSON = parseJSON_Field 

parseJSON_Field :: JasonParser (JasonField k cs kvs)
parseJSON_Field = todo 

^^^ that didn't work because the instantiated type is in positive position, 
like Read not Show, i.e. it produces the record from nothing, 
it can't consume a record with Fields that are holding the correct constraint.  

-}
