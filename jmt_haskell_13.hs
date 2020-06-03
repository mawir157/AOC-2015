import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

import AdventHelper

type Name = String
type Relns = M.Map (Name, Name) Integer

parseLine :: String -> ((Name, Name), Integer)
parseLine s = ((s'!!0, init (s'!!10)), v')
  where s' = splitOn " " s
        v = read (s'!!3) :: Integer
        sgn = s'!!2
        v' = if' (sgn == "gain") v ((-1) * v)

names :: Relns -> [Name]
names m = nub $ map (fst) $ M.keys m

happiness :: Relns -> [Name] -> Integer
happiness r ss = (go r ss') + (go r $ reverse ss')
  where ss' = ss ++ [head ss]
        go r [a] = 0
        go r (a:b:xs) = (fromJust $ M.lookup (a,b) r) + (go r (b:xs))

addMe :: Relns -> Relns
addMe r = foldl go r n
  where n = names r
        go m x = M.insert (x,"Me") 0 (M.insert ("Me",x) 0 m)

main = do
  f <- readFile "input_13.txt"
  let l = lines f
  let s = M.fromList $ map (parseLine) l

  putStr "Part 1: "
  let p = permutations $ names s
  putStrLn $ show $ maximum $ map (happiness s) p

  putStr "Part 2: "
  let s' = addMe s
  let p = permutations $ names s'
  putStrLn $ show $ maximum $ map (happiness s') p
