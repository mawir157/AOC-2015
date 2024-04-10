import Data.List.Split

import AdventHelper

type Reindeer = (String, Integer, Integer, Integer)
name ::Reindeer -> String
name (n,_,_,_) = n

parseLine :: String -> Reindeer
parseLine s = (s'!!0, vel, time, rest)
  where s' = splitOn " " s
        vel = read (s'!!3) ::Integer
        time = read (s'!!6) :: Integer
        rest = read (s'!!13) :: Integer

cycleStats :: Reindeer -> (Integer, Integer) -- (cycle length, cycle distance)
cycleStats (_,vel,tim,rst) = (tim + rst, tim*vel)

travelled :: Integer -> Reindeer -> Integer
travelled time (n,v,t,r) = ((div time cl) * cd) + (q * v)
  where (cl, cd) = cycleStats (n,v,t,r)
        q = min (rem time cl) t

sumPoints :: [Integer] -> [[Integer]] -> [Integer] 
sumPoints x [] = x
sumPoints x (p:ps) = sumPoints (zipWith (+) x p') ps
  where mp = maximum p
        p' = map (\x -> if' (x==mp) 1 0) p

main = do
  f <- readFile "../input/input14.txt"
  let s = map (parseLine) $ lines f

  putStr "Part 1: "
  putStrLn $ show $ maximum $ map (travelled 2503) s

  putStr "Part 2: "
  let k = map (\n -> map (travelled n) s) [1..2503]
  let p = sumPoints (replicate (length s) 0) k
  putStrLn $ show $ maximum p
