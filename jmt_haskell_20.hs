import Data.List

intSqrt :: Integer -> Integer
intSqrt n = head $ dropWhile (\x -> x*x < n) [1,2..]

divisors :: Integer -> [Integer]
divisors n = nub (loDivs ++ hiDivs)
  where loDivs = [ x | x <- [1,2..(intSqrt n)], rem n x == 0]
        hiDivs = [ div n d | d <- loDivs ]

part1 :: Integer -> Integer
part1 n = 10 * (sum $ divisors n)

part2 :: Integer -> Integer
part2 n = 11 * (sum $ first50)
  where first50 = filter (\x -> (div n x) <= 50) $ divisors n

main = do
  let t = 33100000
  let ps = map part1 [1,2..]
  putStr "Part 1: " 
  putStrLn $ show $ (length $ takeWhile (<= t) ps) + 1

  let ps = map part2 [1,2..]
  putStr "Part 2: " 
  putStrLn $ show $ (length $ takeWhile (\x -> x <= t) ps) + 1
 