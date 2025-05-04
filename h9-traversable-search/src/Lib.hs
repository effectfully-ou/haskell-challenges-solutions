{-# LANGUAGE ExistentialQuantification #-}

module Lib
    ( search
    ) where

import           Data.Foldable (find)
import           Data.Sequence

data Some f = forall a. Some (f a)

newtype Bfs a b = Bfs
    { unBfs :: Seq (Some (Bfs a)) -> [a]
    }

roll :: Seq (Some (Bfs a)) -> [a]
roll ctx = case viewl ctx of
    EmptyL         -> []
    Some b :< ctx' -> unBfs b ctx'

instance Functor (Bfs a) where
    fmap f a = pure f <*> a

instance Applicative (Bfs a) where
    pure _ = Bfs roll
    g <*> b = Bfs $ \ctx -> unBfs g $ ctx |> Some b

yield :: a -> Bfs a ()
yield x = Bfs $ (x :) . roll

bfs :: Traversable t => t a -> [a]
bfs xs = unBfs (traverse yield xs) Empty

search :: Traversable t => (a -> Bool) -> t a -> Maybe a
search p = find p . bfs
