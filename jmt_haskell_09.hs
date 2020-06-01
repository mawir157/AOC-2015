import qualified Data.Map as M
import qualified Data.Set as S

import Data.List
import Data.List.Split
import Data.Maybe

type Graph = M.Map (City, City) Integer
type City = String

parseInput :: String -> (City, City, Integer)
parseInput s = (s'!!0, s'!!2, read (s'!!4) :: Integer)
  where s' = splitOn " " s

addToGraph :: Graph -> (City, City, Integer) -> Graph
addToGraph g (x,y,d) = M.insert (y,x) d g'
  where g' = M.insert (x,y) d g

d :: Graph -> City -> City -> Integer
d g from to = fromJust $ M.lookup (from, to) g

allCities :: Graph -> S.Set City
allCities g = S.fromList $ map fst $ M.keys g
--------------------------------------------------------------------------------
bestRoute :: Graph -> Integer
bestRoute g = minimum $ map (minDistance g) pairs
  where cities = S.toList $ allCities g
        pairs = [(x,y) | x <- cities, y <- cities, x /= y]

minDistance :: Graph -> (City, City) -> Integer
minDistance g (from, to) = hk g from (s',to)
  where s = allCities g
        s' = S.delete from s

hk :: Graph -> City -> (S.Set City, City) -> Integer
hk g from (via, to)
  | S.size via == 1 = d g from to -- via = {to}
  | otherwise       = minimum $ map (\(v,x) -> (d g x to) + hk g from (v,x)) dd
  where via' = S.delete to via -- Set City == S/c
        dd = zip (repeat via') (S.toList via') -- [(Set City, City)] == [(S/c, x)]
--------------------------------------------------------------------------------
worstRoute :: Graph -> Integer
worstRoute g = maximum $ map (maxDistance g) pairs
  where cities = S.toList $ allCities g
        pairs = [ (x,y) | x <- cities, y <- cities, x /= y]

maxDistance :: Graph -> (City, City) -> Integer
maxDistance g (from, to) = hkWrong g from (s',to)
  where s = allCities g
        s' = S.delete from s

hkWrong :: Graph -> City -> (S.Set City, City) -> Integer
hkWrong g from (via, to)
  | S.size via == 1 = d g from to
  | otherwise       = maximum $ map (\(v,x) -> (d g x to) + hkWrong g from (v,x)) dd
  where via' = S.delete to via
        dd = zip (repeat via') (S.toList via')
--------------------------------------------------------------------------------
main = do
  f <- readFile "input_09.txt"
  let ss = map parseInput $ lines f
  let g = M.empty :: Graph
  let g' = foldl addToGraph g ss

  putStr "Part 1: "
  putStrLn $ show $ bestRoute g'
  putStr "Part 2: "
  putStrLn $ show $ worstRoute g'
