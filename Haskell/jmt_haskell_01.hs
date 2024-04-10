parsePart1 :: Int -> Char -> Int
parsePart1 d c
  | c == '(' = d + 1
  | c == ')' = d - 1

main = do
  f <- readFile "../input/input01.txt"
  let s = head $ lines f -- [Char]
  putStr "Part 1: "
  putStrLn $ show $ foldl (parsePart1) 0 s

  putStr "Part 2: "
  let t = takeWhile (/= -1) $ scanl (parsePart1) 0 s
  putStrLn $ show $ length t
