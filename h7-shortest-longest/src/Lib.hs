-- Lots of interesting solutions at https://www.reddit.com/r/haskell/comments/nitmvw/shortest_longest

module Lib
    ( shortestLongest
    ) where
import           Data.List

pre :: ((Ordering, [[a]]) -> (Ordering, [[a]])) -> [[a]] -> [[a]]
pre f = foldr step [] where
    step xs0 []   = map pure xs0
    step xs0 yss0 = snd $ go xs0 yss0 where
        go []     []       = (EQ, [])
        go []     yss      = f (GT, yss)
        go xs     []       = f (LT, map pure xs)
        go (x:xs) (ys:yss) = (o, zs:zss) where
            (o, zss) = go xs yss
            zs = case o of
                LT -> x:[]
                EQ -> x:ys
                GT ->   ys

preshortest :: [[a]] -> [[a]]
preshortest = pre (\(o, _) -> (EQ `compare` o, []))

prelongest :: [[a]] -> [[a]]
prelongest = pre id

shortestLongest :: [[[a]]] -> [[a]]
shortestLongest = transpose . map concat . preshortest . map prelongest
