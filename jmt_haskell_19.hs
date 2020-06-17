import Data.List
import Data.List.Split

import Debug.Trace

type Rule = (String, String)

search :: (Eq a) => [a] -> [a] -> [Int]
search sub str = findIndices (isPrefixOf sub) (tails str)

parseInput :: String -> Rule
parseInput s = (ss!!0, ss!!1)
  where ss = splitOn " => " s

applyRule' :: String -> Rule -> Int -> String
applyRule' s (from, to) n = p ++ to ++ q
  where p = take n s
        q = drop (length from) $ drop n s

applyRule :: String -> Rule -> [String]
applyRule s (f,t) = map (\n -> applyRule' s (f,t) n) is
  where is = search f s

-- rep :: String -> ([String],Int) -> [Rules] -> ([String],Int)
-- rep target (seen, count) rules
--   | elem target seen = (seen, count)
--   | otherwise        = 

revRule :: String -> Rule -> [String]
revRule s (f,t) = map (\n -> applyRule' s (t,f) n) is
  where is = search t s

revAllRules :: [Rule] -> String -> [String]
revAllRules rs s = concat $ map (revRule s) rs

reduce :: [String] -> [Rule] -> [String]
reduce seen rules = seen'
  where seen' = nub $ concat $ map (revAllRules rules) seen

reduceToEnd :: ([String],Int) -> [Rule] -> ([String],Int)
reduceToEnd (ss,n) rs
  | elem "e" ss = (ss, n)
  | otherwise   = (reduceToEnd (reduce ss rs, n+1)) rs

main = do
  f <- readFile "input_19.txt"
  let rules = map parseInput $ init $ init $ lines f
  let molecule = last $ lines f

  let rules = [("e","H"),("e","O"),("H","HO"),("H","OH"),("O", "HH")]
  let molecule = "HOHOHO"

  let w = nub $ concat $ map (applyRule molecule) rules

  putStr "Part 1: "
  putStrLn $ show $ length w

  putStr "Part 2: "
  putStrLn $ show $ length molecule
  -- putStrLn $ show $ snd $ reduceToEnd ([molecule], 0) rules

  let t1 = reduce [molecule] rules
  putStrLn $ show $ length t1 
  let t2 = reduce t1 rules
  putStrLn $ show $ length t2 

  let t1 = reduce t2 rules
  putStrLn $ show $ length t1 
  let t2 = reduce t1 rules
  putStrLn $ show $ length t2 

  let t1 = reduce t2 rules
  putStrLn $ show $ length t1 
  let t2 = reduce t1 rules
  putStrLn $ show $ length t2 

  let t1 = reduce t2 rules
  putStrLn $ show $ length t1 
  let t2 = reduce t1 rules
  putStrLn $ show $ length t2 
