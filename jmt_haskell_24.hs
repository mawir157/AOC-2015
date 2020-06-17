import Data.List

subseqOfLength :: Int -> [a] -> [[a]]
subseqOfLength n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
--------------------------------------------------------------------------------
firstThirds :: Int -> [Integer] -> [[Integer]]
firstThirds n ws = qs
  where k = div (sum ws) 3
        g1s = filter (\t -> sum t == k) $ subseqOfLength n ws
        qs = filter (\x -> isHalfable (ws \\ x)) g1s

firstQuarters :: Int -> [Integer] -> [[Integer]]
firstQuarters n ws = qs
  where k = div (sum ws) 4
        g1s = filter (\t -> sum t == k) $ subseqOfLength n ws
        qs = filter (\x -> isTrisectable (ws \\ x)) g1s

isHalfable :: [Integer] -> Bool
isHalfable ws = any (\x -> sum x == k) ts
  where k = div (sum ws) 2
        ts = subsequences ws

isTrisectable :: [Integer] -> Bool
isTrisectable ws = any (isHalfable) ws'
  where k = div (sum ws) 3
        ts = filter (\t -> sum t == k) $ subsequences ws
        ws' = map (\x -> ws \\ x) ts

findBestTriple :: Int -> [Integer] -> [[Integer]]
findBestTriple n ws
  | length k /=0 = k
  | otherwise    = findBestTriple (n+1) ws
  where k = firstThirds n ws

findBestQuarter :: Int -> [Integer] -> [[Integer]]
findBestQuarter n ws
  | length k /=0 = k
  | otherwise    = findBestQuarter (n+1) ws
  where k = firstQuarters n ws
--------------------------------------------------------------------------------
main = do
  f <- readFile "input_24.txt"
  let ws = map (read) $ lines f :: [Integer]

  let rs = findBestTriple 0 ws
  putStr "Part 1: "
  putStrLn $ show $ minimum $ map (product) rs

  let rs = findBestQuarter 0 ws
  putStr "Part 2: "
  putStrLn $ show $ minimum $ map (product) rs

