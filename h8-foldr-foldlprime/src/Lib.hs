{-# LANGUAGE LambdaCase #-}

module Lib
    ( foldr
    ) where

import           Control.Concurrent
import           Data.Foldable      (foldl')
import           Data.Function
import           Data.Functor
import           Prelude            hiding (foldr)
import           System.IO.Unsafe

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f z xs = unsafePerformIO $ do
    request  <- newEmptyMVar
    response <- newEmptyMVar
    _ <- forkIO $ do
        let f' r x = unsafePerformIO $ do
                putMVar response $ Just x
                r <$ takeMVar request
            z' = putMVar response Nothing
        foldl' f' z' xs
    fix $ \r ->
        takeMVar response >>= \case
            Nothing -> pure z
            Just x  -> f x <$> unsafeInterleaveIO (putMVar request () *> r)
