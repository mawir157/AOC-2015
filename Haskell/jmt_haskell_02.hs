import Data.List
import Data.List.Split

parseInput :: String -> (Integer, Integer, Integer)
parseInput s = (read (s'!!0) :: Integer, read (s'!!1) :: Integer, read (s'!!2) :: Integer)
  where s' = splitOn "x" s

paper :: (Integer, Integer, Integer) -> Integer
paper (x,y,z) = 2 * (x*y + y*z + z*x) + slack
  where slack = minimum [x*y, y*z, z*x]

ribbon :: (Integer, Integer, Integer) -> Integer
ribbon (x,y,z) = 2 * (l + w) + bow
  where [l,w] = take 2 $ sort [x,y,z]
        bow = (x*y*z)

main = do
  f <- readFile "../input/input02.txt"
  let s = lines f 

  let dims = map parseInput s

  putStr "Part 1: "
  putStrLn $ show $ sum $ map paper dims

  putStr "Part 2: "
  putStrLn $ show $ sum $ map ribbon dims
