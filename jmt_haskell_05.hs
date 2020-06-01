import Data.List

vowels = "aeiou"
badPairs = ["ab", "cd", "pq", "xy"]

vCount :: String -> Int
vCount s = length $ filter (\c -> elem c vowels) s

hasDouble :: String -> Bool
hasDouble [c] = False
hasDouble (a:b:ss) = (a == b) || hasDouble (b:ss)

badPair :: String -> Bool
badPair [c] = False
badPair (a:b:ss) = (elem [a,b] badPairs) || badPair (b:ss)

repPair :: String -> Bool
repPair [a,b] = False
repPair (a:b:ss)
  | isInfixOf [a,b] ss = True
  | otherwise          = repPair (b:ss)

sandwich :: String -> Bool
sandwich [a,b] = False
sandwich (a:b:c:s)
  | a == c    = True
  | otherwise = sandwich (b:c:s) 

main = do
  f <- readFile "input_05.txt"
  let s = lines f 

  putStr "Part 1: "
  let t = filter (\s -> vCount s >= 3) s
  let u = filter (hasDouble) t
  let v = filter (not . badPair) u
  putStrLn $ show $ length v

  putStr "Part 2: "
  let t = filter repPair s
  let u = filter sandwich t
  putStrLn $ show $ length u
