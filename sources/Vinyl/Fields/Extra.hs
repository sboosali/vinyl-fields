{-# LANGUAGE NoImplicitPrelude, PatternSynonyms, GADTs, DataKinds, PolyKinds #-}
module Vinyl.Fields.Extra
 ( module Prelude.Spiros
 , module Vinyl.Fields.Extra
 ) where

import Prelude.Spiros hiding (I, pattern I, C, pattern C, P, pattern P)
import Data.Functor.Classes

data Uncurry :: (k -> j -> *) -> (k, j) -> * where 
    Uncurry :: f a b -> Uncurry f '(a,b) 

type Showing a = Int -> a -> ShowS 

show1 :: (Show1 f, Show a) => f a -> String
show1 a = showsPrec1 0 a ""

-- showsPrecedent1 :: (Show1 f, Show a) => Int -> f a -> ShowS 
-- showsPrecedent1 p a = showsPrec1 p a ""

the :: forall a. a -> a
the = id
