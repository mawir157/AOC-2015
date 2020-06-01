module AdventHelper where
import Data.List
import Data.List.Split

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

if' True  x _ = x
if' False _ y = y

-- foldl' f z []     = z
-- foldl' f z (x:xs) = let z' = z `f` x 
--                     in seq z' $ foldl' f z' xs