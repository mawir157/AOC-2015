import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.Map as M 

import AdventHelper

data Logic = AND | OR  | NOT | RSHIFT | LSHIFT | SET deriving (Eq, Show)
type Dual = (Bool, Int, String)
emptyDual = (False, 0, "")

type Ins  = (Logic, Dual, Dual, Dual)

toDual :: String -> Dual
toDual s
  | elem (head s) "0123456789" =  (True, read s :: Int, "")
  | otherwise                  = (False, -1, s)

dualMode :: Dual -> Bool
dualMode (b,_,_) = b

dualStr :: Dual -> String
dualStr (_,_,s) = s

parseInput :: String -> (Logic, Dual, Dual, Dual)
parseInput s
  | t!!0 == "NOT"    = (NOT,    toDual (t!!1), emptyDual,     toDual (t!!3))
  | t!!1 == "AND"    = (AND,    toDual (t!!0), toDual (t!!2), toDual (t!!4))
  | t!!1 == "OR"     = (OR,     toDual (t!!0), toDual (t!!2), toDual (t!!4))
  | t!!1 == "RSHIFT" = (RSHIFT, toDual (t!!0), toDual (t!!2), toDual (t!!4))
  | t!!1 == "LSHIFT" = (LSHIFT, toDual (t!!0), toDual (t!!2), toDual (t!!4))
  | t!!1 == "->"     = (SET,    toDual (t!!0), emptyDual,     toDual (t!!2))
  where t = splitOn " " s

getValue :: M.Map String Int -> Dual -> Int
getValue m (b, i, c)
  | b = i
  | elem c (M.keys m) = fromJust $ M.lookup c m

apply :: M.Map String Int -> Ins -> M.Map String Int
apply m (l, a, b, c)
  | l == NOT    = M.insert c' ((complement a') `mod` 65536) m
  | l == AND    = M.insert c' ((.&.) a' b') m
  | l == OR     = M.insert c' ((.|.) a' b') m 
  | l == RSHIFT = M.insert c' (shiftR a' b') m
  | l == LSHIFT = M.insert c' (shiftL a' b') m
  | l == SET    = M.insert c' (a') m
  where a' = getValue m a
        b' = getValue m b
        c' = dualStr c

isValid :: M.Map String Int -> Ins -> Bool
isValid m (l, a, b, c)
  | l == OR || l == AND = aValid && bValid
  | otherwise           = aValid
  where ks = M.keys m
        aValid = if' (dualMode a) (True) (elem (dualStr a) ks)
        bValid = if' (dualMode b) (True) (elem (dualStr b) ks)

run :: ([Ins], M.Map String Int) -> ([Ins], M.Map String Int)
run (ins, gates)
  | length ins == 0 = (ins, gates)
  | otherwise       = run (ins', gates')
  where valid  = filter (isValid gates) ins -- Set Ins
        ins'   = ins \\ valid
        gates' = foldl (apply) gates valid

main = do
  f <- readFile "input_07.txt"
  let s = lines f 

  let i = map (parseInput) s
  let m = M.empty :: M.Map String Int

  let (i', m')  = run (i,m)
  putStr "Part 1: "
  let a = fromJust $ M.lookup "a" m'
  putStrLn $ show a

  -- remove the previous write to b instruction and replace it
  let i2 = filter (\(_,_,_,(_,_,c)) -> c /= "b") i
  let i2' = i2 ++ [(SET,(True,a,""),(False,0,""),(False,-1,"b"))]

  putStr "Part 2: "
  let m = M.singleton "a" a
  let (i', m')  = run (i2',m)
  let a = fromJust $ M.lookup "a" m'
  putStrLn $ show a
