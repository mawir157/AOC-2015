module AdventHelper where
import Data.List
import Data.List.Split

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

if' True  x _ = x
if' False _ y = y

-- b^a mod p
modPow :: Integer -> Integer -> Integer -> Integer
modPow b 0 p = 1
modPow b 1 p = mod b p
modPow b a p | even a = mod ((modPow b (div a 2) p) ^ 2) p
             | odd  a = mod ((modPow b (div (a-1) 2) p) ^ 2 * b) p