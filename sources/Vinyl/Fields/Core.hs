{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds, PatternSynonyms #-}
module Vinyl.Fields.Core where
-- import Vinyl.Fields.Extra
-- import Vinyl.Fields.Types

-- import Prelude.Spiros

import Data.Proxy
-- import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Const
-- import Data.Functor.Compose
-- import Data.Functor.Product
-- import Data.Functor.Sum

type I = Identity

pattern I :: forall a. a -> Identity a
pattern I a = Identity a

type C = Const

pattern C :: forall b a. b -> Const b a
pattern C a = Const a

type P = Proxy

pattern P :: forall a. Proxy a
pattern P = Proxy

