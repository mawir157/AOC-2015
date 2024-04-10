import Data.Maybe
import qualified Data.Map as M

import AdventHelper

data Code = HLF | TPL | INC | JMP | JIE | JIO deriving (Eq, Show) 
type RegArr = M.Map Char Int
type CPU = (RegArr, Int)

type Ins = (Code, Char, Int)

parseInt :: String -> Int
parseInt s
  | head s == '+' = read (drop 1 s) :: Int
  | head s == '-' = -1 * (read (drop 1 s) :: Int)
  | otherwise     = read s :: Int

parseLine :: String -> Ins
parseLine s
  | code == "hlf" = (HLF, last s, 0)
  | code == "tpl" = (TPL, last s, 0)
  | code == "inc" = (INC, last s, 0)
  | code == "jmp" = (JMP, '_', parseInt d4)
  | code == "jie" = (JIE, s!!4, parseInt d7)
  | code == "jio" = (JIO, s!!4, parseInt d7)
  where code = take 3 s
        d4 = drop 4 s
        d7 = drop 7 s

applyIns :: CPU -> Ins -> CPU
applyIns (regs, ptr) (c, r, v)
  | c == HLF = (M.insert r (div rv 2) regs, ptr + 1)
  | c == TPL = (M.insert r (3 * rv) regs, ptr + 1)
  | c == INC = (M.insert r (1 + rv) regs, ptr + 1)
  | c == JMP = (regs, ptr + v)
  | c == JIE = (regs, if' (even rv) (ptr+v) (ptr+1))
  | c == JIO = (regs, if' (rv == 1) (ptr+v) (ptr+1))
  where rv = M.findWithDefault 0 r regs

run :: [Ins] -> CPU -> CPU
run r (c,p)
  | p >= length r = (c,p)
  | otherwise     = run r $ applyIns (c,p) (r!!p)

main = do
  f <- readFile "../input/input23.txt"
  let is = map parseLine $ lines f

  let (r,p) = run is (M.empty, 0)
  putStr "Part 1: "
  putStrLn $ show $ fromJust $ M.lookup 'b' r

  let (r,p) = run is (M.singleton 'a' 1, 0)
  putStr "Part 2: "
  putStrLn $ show $ fromJust $ M.lookup 'b' r
