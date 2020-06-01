step :: String -> String
step [] = []
step s = (show n) ++ (id $ [head s]) ++ (step $ drop n s)
  where h = takeWhile (== head s) s
        n = length h

repSteps :: Int -> String -> String
repSteps n s
  | n == 0    = s
  | otherwise = repSteps (n-1) $ step s

main = do
  let i = "1321131112"
  putStr "Part 1: "
  putStrLn $ show $ length $ repSteps 40 i
  putStr "Part 2: "
  putStrLn $ show $ length $ repSteps 50 i
