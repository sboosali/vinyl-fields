{-# LANGUAGE NoImplicitPrelude, PatternSynonyms #-}
module Vinyl.Fields.Extra
 ( module Prelude.Spiros
 , module Vinyl.Fields.Extra
 ) where

import Prelude.Spiros hiding (I, pattern I, C, pattern C, P, pattern P)
import Data.Functor.Classes

show1 :: (Show1 f, Show a) => f a -> String
show1 a = showsPrec1 0 a ""

the :: forall a. a -> a
the = id
