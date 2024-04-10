import Data.List

type Pos = (Integer, Integer)

move :: Pos -> Char -> Pos
move (x,y) c
  | c == '^' = (x+1,y)
  | c == '<' = (x,y-1)
  | c == '>' = (x,y+1)
  | c == 'v' = (x-1,y)

alt :: [a] -> [a]
alt [] = []
alt (x:xs) = x : (alt $ drop 1 xs)

everyOther :: [a] -> ([a],[a])
everyOther x = (alt x, alt $ drop 1 x) 

main = do
  f <- readFile "../input/input03.txt"
  let s = head $ lines f 

  let ps = scanl move (0,0) s

  putStr "Part 1: "
  putStrLn $ show $ length $ nub ps

  putStr "Part 2: "
  let (s1, s2) = everyOther s
  let santa = scanl move (0,0) s1
  let roboS = scanl move (0,0) s2
  putStrLn $ show $ length $ nub (santa ++ roboS)
