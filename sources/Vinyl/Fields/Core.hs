{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module Vinyl.Fields.Core where
-- import Vinyl.Fields.Extra
-- import Vinyl.Fields.Types

-- import Prelude.Spiros
import Data.Functor.Identity

type I = Identity

pattern I :: forall a. a -> Identity a
pattern I a = Identity a
