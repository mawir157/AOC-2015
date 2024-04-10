import AdventHelper

coordsToInt :: Integer -> Integer -> Integer
coordsToInt r c = (nthTri ((r-1) + c)) - (r-1)
  where nthTri n = div (n * (n + 1)) 2

modLin :: Integer -> Integer
modLin n = mod ((modPow 252533 (n-1) 33554393) * 20151125) 33554393

main = do
  putStr "Part 1: "
  putStrLn $ show $ modLin $ coordsToInt 2978 3083
