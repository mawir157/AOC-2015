import Data.List

main = do
  f <- readFile "input_17.txt"
  let s = map (read) $ lines f :: [Integer]
  let t = 150

  putStr "Part 1: "
  let a = filter (\x -> sum x == t) $ subsequences s
  putStrLn $ show $ length a

  putStr "Part 2: "
  let m = minimum $ map (length) a
  let b = filter (\x -> length x == m) a
  putStrLn $ show $ length b
