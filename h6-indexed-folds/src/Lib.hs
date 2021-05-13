{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Lib
    ( Nat (..)
    , Vec (..)
    , ifoldl'
    ) where

import           Data.Coerce
import           Data.Functor.Compose

data Nat = Z | S Nat

data Vec n a where
    Nil  :: Vec 'Z a
    Cons :: a -> Vec n a -> Vec ('S n) a

deriving instance Foldable (Vec n)

ifoldr :: forall b n a. (forall m. a -> b m -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldr f z = go where
    go :: Vec m a -> b m
    go Nil         = z
    go (Cons x xs) = f x $ go xs
{-# INLINE ifoldr #-}

newtype IFoldlMotive a n = IFoldlMotive
    { unIFoldlMotive :: forall b. (forall m. b m -> a -> b ('S m)) -> b 'Z -> b n
    }

ifoldl' :: (forall m. b m -> a -> b ('S m)) -> b 'Z -> Vec n a -> b n
ifoldl' f0 z0 xs = unIFoldlMotive (ifoldr fm zm xs) f0 z0 where
    fm x (IFoldlMotive h) = IFoldlMotive $ \f z ->
        getCompose $ h (coerceF f) $! Compose (f z x)
    zm = IFoldlMotive $ \f z -> z

     -- A simplified version by David Feuer: https://gist.github.com/effectfully/fa8a4d1da8d7ffad199451ce85b15c91#gistcomment-3739350
    coerceF
        :: (b ('S p) -> a -> b ('S ('S p)))
        -> Compose b 'S p -> a -> Compose b 'S ('S p)
    coerceF = coerce
