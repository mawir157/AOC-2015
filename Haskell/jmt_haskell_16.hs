import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe

type Sue = M.Map String Integer

parseLine :: String -> M.Map String Integer
parseLine s = M.fromList ([("Sue", sID)] ++ l)
  where sn = takeWhile (/= ':') s -- "Sue XXX"
        sID = read (drop 4 sn) :: Integer
        tl = drop 2 $ dropWhile (/= ':') s -- "cars: 5, vizslas: 3, children: 10"
        k = splitOn ", " tl -- ["cars: 5", "vizslas: 3", "children: 10"]
        k' = map (\st -> splitOn ": " st) k -- [["cars","5"],["vizslas", "3"],["children", "10"]]
        l = map(\xs -> (xs!!0, read (xs!!1) :: Integer)) k'

okSue :: Sue -> Sue -> (Bool, Integer)
okSue m s = (good, fromJust $ M.lookup "Sue" s)
  where t = M.differenceWith (\v w -> Just (v-w)) s m
        good = all (== 0) $ M.elems $ M.delete "Sue" t

eq :: Sue -> Sue -> String -> Bool
eq m s k
  | M.notMember k s = True
  | elem k ["cats", "trees"]           = mv <  sv
  | elem k ["pomeranians", "goldfish"] = mv >  sv
  | otherwise                          = mv == sv
  where mv = fromJust $ M.lookup k m
        sv = fromJust $ M.lookup k s

okSue2 :: Sue -> Sue -> Bool
okSue2 m s = all (eq m s) $ M.keys m

main = do
  f <- readFile "../input/input16.txt"
  let s = map parseLine $ lines f

  let match = M.fromList [("children", 3),
                          ("cats", 7),
                          ("samoyeds", 2),
                          ("pomeranians", 3),
                          ("akitas", 0),
                          ("vizslas", 0),
                          ("goldfish", 5),
                          ("trees", 3),
                          ("cars", 2),
                          ("perfumes", 1)]

  let ss = filter (\x -> fst $ okSue match x) s
  putStr "Part 1: "
  putStrLn $ show $ head $ map (fromJust . M.lookup "Sue") ss

  let p2 = filter (\x -> okSue2 match x)  s
  putStr "Part 2: "
  putStrLn $ show $ head $ map (fromJust . M.lookup "Sue")  p2
