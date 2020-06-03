import Data.List

parseHelper :: ([Integer], String) -> ([Integer], String)
parseHelper (xs,[])  = (xs, [])
parseHelper (xs,ss)
  | length k == 0 = (xs, [])
  | otherwise     =  parseHelper (xs ++ [read t :: Integer], drop (length t) k)
  where k = dropWhile (\x -> not $ elem x "-0123456789") ss
        t = takeWhile (\x -> elem x "-0123456789") k

parseInput :: String -> [Integer]
parseInput s = fst $ parseHelper ([],s)

main = do
  f <- readFile "input_12.txt"
  let ss = map parseInput $ lines f
  putStr "Part 1: "
  putStrLn $ show $ map sum ss
