import Numeric
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C

hashString :: String -> String
hashString s = show (hashWith MD5 i)
  where i = C.pack s

leadingZeros :: String -> Int
leadingZeros s = length $ takeWhile (== '0') s

isGood :: Int -> String -> Integer -> Bool
isGood l s n = (leadingZeros h) >= l
  where h = hashString (s ++ (show n))

main = do
  let i = "yzbqklnj"
  let k = take 1 $ dropWhile (not . isGood 5 i) [1,2..]
  putStr "Part 1: "
  putStrLn $ show k

  let k = take 1 $ dropWhile (not . isGood 6 i) [1,2..]
  putStr "Part 1: "
  putStrLn $ show k
