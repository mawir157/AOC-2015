import qualified Data.Map as M
import Data.List
import AdventHelper

data Mode = ON | OFF | TOGGLE deriving (Show, Eq)
type Pos = (Integer, Integer)
type Ins = (Mode, Pos, Pos)

parseInput :: String -> Ins
parseInput s = toIns s'
  where s' =  splitOnAnyOf [" ", ","] s

toIns :: [String] -> Ins
toIns ss
  | ss!!0 == "toggle" = (TOGGLE, 
                         (read (ss!!1) :: Integer, read (ss!!2) :: Integer),
                         (read (ss!!4) :: Integer, read (ss!!5) :: Integer))
  | ss!!1 == "on"     = (ON, 
                         (read (ss!!2) :: Integer, read (ss!!3) :: Integer),
                         (read (ss!!5) :: Integer, read (ss!!6) :: Integer))
  | ss!!1 == "off"    = (OFF, 
                         (read (ss!!2) :: Integer, read (ss!!3) :: Integer),
                         (read (ss!!5) :: Integer, read (ss!!6) :: Integer))
  | otherwise = error (show $ length ss)
--------------------------------------------------------------------------------
initGrid :: Integer -> Integer -> M.Map Pos Bool
initGrid x y = M.fromList g
  where ps = [ (x',y') | x' <- [0..(x-1)], y' <- [0..(y-1)] ]
        g = zip ps (repeat False)

updateGrid :: M.Map Pos Bool -> Ins -> M.Map Pos Bool
updateGrid m (md, (x1,y1), (x2,y2))
  | md == OFF    = foldl (\m' p -> M.insert p False m') m rect
  | md == ON     = foldl (\m' p -> M.insert p True m')  m rect
  | md == TOGGLE = foldl (\m' p -> M.adjust (\a -> not a) p m') m rect
  where rect = [ (x',y') | x' <- [x1..x2], y' <- [y1..y2] ]
--------------------------------------------------------------------------------7
initGrid2 :: Integer -> Integer -> M.Map Pos Integer
initGrid2 x y = M.fromList g
  where ps = [ (x',y') | x' <- [0..(x-1)], y' <- [0..(y-1)] ]
        g = zip ps (repeat 0)

updateGrid2 :: M.Map Pos Integer -> Ins -> M.Map Pos Integer
updateGrid2 m (md, (x1,y1), (x2,y2))
  | md == OFF    = foldl' (\m' p -> M.adjust (\b -> max 0 (b-1)) p m')  m rect
  | md == ON     = foldl' (\m' p -> M.adjust (\b -> b+1) p m') m rect
  | md == TOGGLE = foldl' (\m' p -> M.adjust (\b -> b+2) p m') m rect
  where rect = [ (x',y') | x' <- [x1..x2], y' <- [y1..y2] ]
--------------------------------------------------------------------------------
main = do
  f <- readFile "input_06.txt"
  let s = lines f 

  let i = map (parseInput) s
  putStr "Part 1: "
  let g = initGrid 1000 1000
  let g' = foldl' updateGrid g i
  putStrLn $ show $ M.size $ M.filter (\a -> a) g'

  putStr "Part 2: "
  let g = initGrid2 1000 1000
  let g' = foldl' updateGrid2 g i
  putStrLn $ show $ sum $ M.elems g'