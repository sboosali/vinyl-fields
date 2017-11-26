-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module Vinyl.Fields.Example where
import Vinyl.Fields
import System.Environment

{-|

@
stack build && stack exec -- example-vinyl-fields
@

-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith s = do
 putStrLn s
 putStrLn $ displayRecord         dog_TypeApplications
 putStrLn $ displayIdentityRecord dog_TypeApplications

type Dog f = 
    Record '[Show] f ["name" ::: String, "age" ::: Int]

dog_TypeApplications = 
    field @"name" (I "loki") :* field @"age" (I @Int 7) :* R

-- dog_ifield = ifield @"name" "loki" :* ifield @"age" @Int 7 :* R

-- dog_XOverloadedLabels = (#name -: "loki") :* R -- #age -: 7 :* R
