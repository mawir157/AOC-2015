import Data.Maybe
import qualified Data.Map as M

import AdventHelper

type Pos = (Int, Int) 
type Grid = M.Map Pos Bool

parseLine :: Int -> String -> Grid
parseLine n s = M.fromList $ zip ps s'
  where s' = map (\x -> x == '#') s -- [Bool]
        ps = [ (n, x) | x <- [0,1..] ]

parseGrid :: Int -> [String] -> Grid
parseGrid _ [] = M.empty :: Grid
parseGrid n (s:ss) = M.union (parseLine n s) (parseGrid (n+1) ss)

sLookup :: Grid -> Pos -> Integer
sLookup m p
  | M.member p m = if' (fromJust $ M.lookup p m) 1 0 
  | otherwise    = 0

nbrs :: Pos -> [Pos]
nbrs (x,y) = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),
              (x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]

nCount :: Grid -> Pos -> Integer
nCount m p = sum bs
  where bs = map (sLookup m) $ nbrs p -- [Integer]

updateLife :: Grid -> Grid -> Pos -> Grid
updateLife prev m p
  | st == 1   = M.insert p (if' (count == 2 || count == 3) True False) m
  | otherwise = M.insert p (if' (count == 3) True False) m
  where st = sLookup prev p
        count = nCount prev p

setToTrue :: Grid -> Pos -> Grid
setToTrue m p = M.insert p True m

setCornersOn :: Grid -> Grid
setCornersOn g = foldl setToTrue g corners
  where keys = M.keys g
        mx = maximum $ map fst keys
        my = maximum $ map snd keys
        corners = [(0,0), (0,my), (mx,0), (mx,my)]

click :: Grid -> Grid
click m = foldl (updateLife m) m ps 
  where ps = M.keys m

clickRep :: Int -> Grid -> Grid
clickRep 0 g = g
clickRep n g = clickRep (n-1) $ click g

clickRep2 :: Int -> Grid -> Grid
clickRep2 0 g = g
clickRep2 n g = clickRep2 (n-1) $ setCornersOn $ click g

main = do
  f <- readFile "../input/input18.txt"
  let s = parseGrid 0 $ lines f

  let s' = clickRep 100 s
  let v = length $ filter (\x -> x) $ M.elems s'
  putStr "Part 1: "
  putStrLn $ show v

  let s' = clickRep2 100 (setCornersOn s)
  let v = length $ filter (\x -> x) $ M.elems s'
  putStr "Part 2: "
  putStrLn $ show v