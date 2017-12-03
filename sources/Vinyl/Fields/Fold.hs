{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase, ViewPatterns  #-}
{-# LANGUAGE TypeOperators, TypeApplications, DataKinds #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds, PolyKinds #-} 
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

{-| 

-}
module Vinyl.Fields.Fold where
import Vinyl.Fields.Extra
import Vinyl.Fields.Types 

--import qualified Control.Foldl as L
import Control.Foldl (Fold(..)) 

type URecord_ p = Record '[] (Uncurry p)

type FolderRecord = Record '[] (Uncurry Fold)

-- foldRecord :: FolderRecord fields -> [IRecord_ inputs] -> IRecord_ outputs 
-- foldRecord = todo

-- applyRecord
--   :: forall p f g as bs.
--      ( 
--      )
--   => (forall x y. p x y -> f x -> g y)
--   -> URecord_ p (ZipTypes as bs) -> Record_ f as -> Record_ g bs
-- applyRecord u = \case
--   R                         -> \R               -> R
--   (Field (Uncurry p) :* ps) -> \(Field a :* as) -> u p a :* applyRecord u ps as

{-NOTE 

the inference flows from the `as` plus the `bs` with: 

  :: ( abs ~ ZipTypes as bs 
     )

the inference flows from the `abs` with: 

  :: ( as ~ GetInputs  abs
     , bs ~ GetOutputs abs
     )

-}

{- 

A 'Fold a b' processes elements of type 'a'
and results in a value of type 'b' 

data Fold a b 
    Fold (x -> a -> x) x (x -> b)
    
fold     :: Foldable f => Fold a b -> f a -> b
fold @[] ::              (Fold a b) -> ([a] -> b) 

mconcat :: Monoid a => Fold a a
foldMap :: Monoid w => (a -> w) -> (w -> b) -> Fold a b

mean :: Fractional a => Fold a a
maximum :: Ord a => Fold a (Maybe a)
minimum :: Ord a => Fold a (Maybe a)
set :: Ord a => Fold a (Set a)

-}

--------------------------------------------------------------------------------

{-

type Fold' = Flip L.Fold 

-}