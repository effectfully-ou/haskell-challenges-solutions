{-
-- A neat solution that is not optimal due to unnecessary sharing.
module Lib
    ( Iterable (..)
    , largestPowersInt
    ) where

import           Data.List
import           Data.Semigroup

class Eq a => Iterable a where
    zer :: a
    inc :: a -> a
    dec :: a -> a

instance Iterable Int where
    zer = 0
    inc = succ
    dec = pred

largestPowersInt :: Int -> [Int]
largestPowersInt = largestPowers

largestPowers :: Iterable a => Int -> [a]
largestPowers k = go id (inc zer) [] where
    go xs d = xs' . go (xs . xs') (inc d) where
        -- Yes, doing your own 'stimes' is more efficient than using the one from 'base',
        -- 'cause for some weird reason the instance for @(->)@ tries to be clever and share things.
        xs' = foldr (.) id $ replicate (k - 1) ((d :) . xs)
-}

-- A fast solution that does not do unnecessary sharing and instead generates the numbers directly.
module Lib
    ( Iterable (..)
    , largestPowersInt
    ) where

import           Data.List
import           Data.Semigroup

class Eq a => Iterable a where
    zer :: a
    inc :: a -> a
    dec :: a -> a

instance Iterable Int where
    zer = 0
    inc = succ
    dec = pred

largestPowersInt :: Int -> [Int]
largestPowersInt = largestPowers

largestPowers :: Iterable a => Int -> [a]
largestPowers k = inf one [] where
    one = inc zer
    inf d = fin (k - 1) d . (inc d :) . inf (inc d) where
        fin r = foldr (.) id . fins r
        fins r d
            | d == one  = replicate (k - 1) (one :)
            | otherwise = intersperse (d :) . replicate r $ fin k (dec d)
