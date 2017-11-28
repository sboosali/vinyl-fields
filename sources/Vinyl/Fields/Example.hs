{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Vinyl.Fields.Example where
import Vinyl.Fields
import Vinyl.Fields.Json  

import System.Environment
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy (ByteString) 
import Data.Functor.Identity
-- import GHC.TypeLits

-- | for inference under `OverloadedStrings`, avoids the spurious @Defaulting to String@ warnings.
s :: String -> String 
s = id 

{-|

@
stack build && stack exec -- example-vinyl-fields
@

-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [x] -> return (x)
  _ -> return ("")
 mainWith arguments

mainWith x = do
 putStrLn x
-- -- putStrLn $ displayRecord         dog_TypeApplications
--  putStrLn $ displayIdentityRecord dog_TypeApplications
--  -- putStrLn $ displayIdentityRecord dog_Identity
--  putStrLn $ displayIdentityRecord dog_ifield
--  putStrLn $ displayIdentityRecord dog_XOverloadedLabels
--  putStrLn $ displayIdentityRecord dog_XOverloadedLabels_Identity
--  putStrLn $ displayIdentityRecord dog_XOverloadedLabels_polymorphic

 putStrLn ""
 putStrLn $ displayIdentityRecord dog_XOverloadedLabels_Identity

 putStrLn ""
 putStrLn $ displayRecord dog_TypeApplications
-- putStrLn $ displayRecord dog_Identity
 putStrLn $ displayRecord dog_ifield
 putStrLn $ displayRecord dog_XOverloadedLabels
 putStrLn $ displayRecord dog_XOverloadedLabels_Identity
-- putStrLn $ displayRecord dog_XOverloadedLabels_polymorphic

 putStrLn ""
 -- print $ dog_XOverloadedLabels_Identity

 -- print (dog_XOverloadedLabels_Identity :: Dog I)
--  print $ dropConstraints' dog_XOverloadedLabels_Identity1 
--  print $ unconstrained    dog_XOverloadedLabels_Identity2 
--  print $ constrain @Show  dog_XOverloadedLabels_Identity3 
--  print $ dropConstraints' dog_XOverloadedLabels_Identity' 
--  print $ unconstrained    dog_XOverloadedLabels_Identity' 
--  print $ constrain @Show  dog_XOverloadedLabels_Identity'
 
 print $ unconstrained    dog_XOverloadedLabels_Identity1 
 print $ constrain' @Show dog_XOverloadedLabels_Identity2

 putStrLn ""
 print $ (fmap . fmap) (constrain' @Show) dog_XOverloadedLabels_list

 putStrLn ""
 print $ proxyDog 
 print $ showableDog_constrained 
--  print $ showableDog_Dictionary 
 print $ maybeDog_Annotated  
 print $ maybeDog_TypeApplications  

 putStrLn ""
 B.putStrLn $ dog_ByteString
 print $ dog_ByteString
 print $ dog_Jason 

 putStrLn "" 
 
 
type Dog = ["name" ::: String, "age" ::: Int]

proxyDog = proxyRecord @Dog 

showableDog_constrained = constrainedRecord @Show @Dog 

-- showableDog_Dictionary = dictionaryRecord @Show @Dog 

maybeDog_Annotated = rPure Nothing :: Record_ Maybe Dog 

maybeDog_TypeApplications = rPure @Dog Nothing 

dog_Readable
  = Field @"name" (Identity (s "Loki")) 
 :* Field @"age"  (Identity 7) 
 :* R

dog_Annotated :: Record '[Show] I Dog 
dog_Annotated =
    field @"name" (I (s "Loki")) :* field @"age" (I @Int 7) :* R

dog_TypeApplications = 
    field @"name" (I (s "Loki")) :* field @"age" (I @Int 7) :* R

-- dog_Identity = 
--     field @"name" (s "Loki") *** field @"age" @Int 7 *** R

dog_ifield = 
    ifield @"name" (s "Loki") :* ifield @"age" @Int 7 :* R

dog_XOverloadedLabels = 
    (#name -: (I (s "Loki"))) :* (#age -: (I @Int 7)) :* R

-- dog_XOverloadedLabels_polymorphic = 
--     (#name =: (s "Loki")) :* (#age =: 7) :* R -- defaults to integer, i.e. infers correctly

dog_XOverloadedLabels_Identity = 
    (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R

-- tests precedent/grouping of the show instance 
dog_XOverloadedLabels_list = 
    [ Right $ (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R
    , Right $ (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R
    , Left (s "") 
    ]

dog_XOverloadedLabels_Identity1 = 
    (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R

dog_XOverloadedLabels_Identity2 = 
    (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R

dog_XOverloadedLabels_Identity3 = 
    (#name =: (s "Loki")) :* (#age =: (7::Int)) :* R

{-

@
{ name = (s "Loki")
, age  = 7
}
@

-}
dog_XOverloadedLabels_Identity' 
    =  #name =: (s "Loki")
  ***  #age  =: (7::Int)
  ***  R

dog_ByteString :: ByteString
dog_ByteString = "{ \"name\": \"loki\", \"age\": 7 }"

dog_Jason :: Either String (R Dog) 
dog_Jason = decodeRecord dog_ByteString

