reduceString :: String -> Int
reduceString []  = 0
reduceString [s] = 1
reduceString (s:t:ss)
  | s /= '\\'              = 1 + reduceString (t:ss)
  | t == '\\' || t == '\"' = 1 + reduceString ss
  | t == 'x'               = 1 + reduceString (drop 2 ss)

expandString :: String -> Int
expandString []  = 2 -- for the quotes
expandString (s:ss)
  | s == '\\' = 2 + expandString ss
  | s == '\"' = 2 + expandString ss
  | otherwise = 1 + expandString ss

trimQuotes :: String -> String
trimQuotes ss = init $ tail ss

main = do
  f <- readFile "../input/input08.txt"
  let ss = lines f 

  let t = map (\s -> (length s, reduceString $ trimQuotes s)) ss
  putStr "Part 1: "
  putStrLn $ show $ sum $ map (\(x,y) -> x - y) t

  putStr "Part 2: "
  let t = map (\s -> (length s, expandString s)) ss
  putStrLn $ show $ sum $ map (\(x,y) -> y - x) t
