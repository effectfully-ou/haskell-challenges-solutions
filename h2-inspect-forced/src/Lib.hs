-- See also this very cool solution (it does not pass the tests though, for a stupid reason,
-- see the discussion): https://www.reddit.com/r/haskell/comments/kz6t1g/challenge_inspect_what_a_function_forces/gjrnujl/

module Lib
    ( Tree (..)
    , materializeForcedBy
    ) where

import           Control.Concurrent
import           Control.Exception
import           Data.IORef
import           System.IO.Unsafe

data Tree
    = Leaf
    | Fork Tree Int Tree
    deriving (Show, Eq)

materializeForcedBy :: (Tree -> Int) -> Tree -> Tree
materializeForcedBy f t = unsafePerformIO $ do
    isForcedVar <- newIORef False

    let annotate :: a -> IO a
        annotate x = unsafeInterleaveIO $ x <$ writeIORef isForcedVar True

        annotateTree :: Tree -> IO Tree
        annotateTree tree = unsafeInterleaveIO $ case tree of
            Leaf       -> pure Leaf
            Fork l x r -> annotate =<< Fork <$> annotateTree l <*> annotate x <*> annotateTree r

        isForcedM :: IO Bool
        isForcedM = readIORef isForcedVar <* writeIORef isForcedVar False

        materialize :: Tree -> IO Tree
        materialize Leaf         = pure Leaf
        materialize (Fork l x r) = do
            isForcedTree <- isForcedM
            if isForcedTree
                then pure Leaf
                else do
                    _ <- evaluate x
                    isForcedValue <- isForcedM
                    lM <- materialize l
                    rM <- materialize r
                    pure $ Fork lM (if isForcedValue then 0 else x) rM

    tA <- annotateTree t
    _ <- evaluate $ f tA
    _ <- isForcedM
    materialize tA
