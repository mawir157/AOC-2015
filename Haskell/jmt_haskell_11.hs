ascTriple :: (Enum a, Eq a) => [a] -> Bool
ascTriple [a,b] = False
ascTriple (a:b:c:xs) = ok || ascTriple (b:c:xs)
  where ok = (succ a == b) && (succ b == c)

noBadChars :: (Eq a) => [a] -> [a] -> Bool
noBadChars bad s = not $ any (\c -> elem c bad) s

pairCount :: (Eq a) => [a] -> Integer
pairCount [] = 0
pairCount [a] = 0
pairCount (a:b:xs)
  | a == b = 1 + pairCount xs
  | otherwise = pairCount (b:xs)

twoPair :: (Eq a) => [a] -> Bool
twoPair s = (pairCount s) >= 2

isPassword :: (Enum a, Eq a) => [a] -> [a] -> Bool
isPassword bad s = (ascTriple s) && (noBadChars bad s) && (twoPair s)

encode :: Int ->  Int -> [Int]
encode base n
  | n == 0    = []
  | otherwise = (encode base $ div n base) ++ [mod n base]

decode :: Int -> [Int] -> Int
decode base xs = fst $ foldr (\a (b, i) -> (b + a * base^i, i + 1)) (0,0) xs

toString :: [Int] -> String
toString [] = []
toString (x:xs) = [toEnum (x + 97) ::Char ] ++ toString xs

fromString :: String -> [Int]
fromString [] = []
fromString (x:xs) = [(fromEnum x) - 97] ++ fromString xs

nextPassword :: Int -> Int
nextPassword n
  | isPassword (fromString "iol") l = n
  | otherwise              = nextPassword (n+1)
  where l = encode 26 n

main = do
  let i = "hepxcrrq"
  let j = nextPassword $ decode 26 $ fromString i
  putStr "Part 1: "
  let i' = toString $ encode 26 j
  putStrLn $ show $ i'

  putStr "Part 2: "
  let j' = nextPassword $ (j+1)
  let i'' = toString $ encode 26 j'
  putStrLn $ show $ i''
