import Data.List
import Data.List.Split

type Ingredient = (String, [Integer], Integer)
mid (_,xs,_) = xs
cal (_,_,c) = c

parseInput :: String -> Ingredient
parseInput s = (init (t!!0), [cap, dur, fla, txt], cal)
  where t = splitOn " " s
        cap = read (init (t!!2)) :: Integer
        dur = read (init (t!!4)) :: Integer
        fla = read (init (t!!6)) :: Integer
        txt = read (init (t!!8)) :: Integer
        cal = read (t!!10) :: Integer

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs)
  | x <= 0    = 0
  | otherwise = x * (product' xs)

score :: [Ingredient] -> [Integer] -> Integer
score is vs = product' $ foldl1 (zipWith (+)) t
  where m = map mid is -- [[Integer]]
        s = zip vs m -- [(Integer, [Integer])]
        t = map (\(i,j) -> map (* i) j) s -- [[Integer]]

calorieCount :: [Ingredient] -> [Integer] -> Integer
calorieCount is vs = sum $ zipWith (*) cs vs 
  where cs = map cal is

hack2 :: [[Integer]]
hack2 = [ [x,100-x] | x <- [1..99] ]

hack4 :: [[Integer]]
hack4 = [ [x,y,z,100-x-y-z] | x <- [1..97], y <- [1..97], z <- [1..97] ]

main = do
  f <- readFile "input_15.txt"
  let s = map parseInput $ lines f

  putStr "Part 1: "
  putStrLn $ show $ maximum $ map (score s) hack4

  putStr "Part 2: "
  let q = filter (\i -> calorieCount s i == 500) hack4
  putStrLn $ show $ maximum $ map (score s) q
