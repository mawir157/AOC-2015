import Data.List
import AdventHelper

 -- Hp Mana Active
type Player = (Integer, Integer, (Integer, Integer, Integer))
-- HP Att
type NPC = (Integer, Integer)

data Spell = MISSLE | DRAIN | SHIELD | POISON | CHARGE deriving (Eq, Show)

type GameState = (Player, NPC, Flag)
data Flag = WIN | LOSE | CONTINUE | DEBUG deriving (Eq, Show)

cost :: Spell -> Integer
cost s
  | s == MISSLE = 53
  | s == DRAIN  = 73
  | s == SHIELD = 113
  | s == POISON = 173
  | s == CHARGE = 229

applyEffects :: GameState -> GameState
applyEffects ((pHP, pMana, (cdSH, cdPO, cdRE)), (bHP, bAtt),flag)
  | flag == CONTINUE = ((pHP, pMana', newCd), (bHP', bAtt), flag')
  | otherwise        = ((pHP, pMana, (cdSH, cdPO, cdRE)), (bHP, bAtt), flag)
  where newCd = (max (cdSH-1) 0 , max (cdPO-1) 0, max (cdRE-1) 0)
        bHP' = bHP - (if' (cdPO > 0) 3 0)
        pMana' = pMana + (if' (cdRE > 0) 101 0)
        flag' = if' (bHP' <= 0) WIN CONTINUE

castSpell :: GameState -> Spell -> GameState
castSpell ((pHP, pMana, (cdSH,cdPO,cdRE)), (bHP,bAtt), flag) sp
  | flag /= CONTINUE = ((pHP, pMana, (cdSH,cdPO,cdRE)), (bHP,bAtt), flag)
  | sp == MISSLE     = ((pHP, pMana - c, (cdSH, cdPO, cdRE)),
                        (bHP - 4, bAtt), if' (bHP < 4) WIN CONTINUE)
  | sp == DRAIN      = ((pHP + 2, pMana - c, (cdSH, cdPO, cdRE)),
                        (bHP - 2, bAtt), if' (bHP < 2) WIN CONTINUE)
  | sp == SHIELD     = ((pHP, pMana - c, (6, cdPO, cdRE)),
                        (bHP, bAtt), flag)
  | sp == POISON     = ((pHP, pMana - c, (cdSH, 6, cdRE)),
                        (bHP, bAtt), flag)
  | sp == CHARGE     = ((pHP, pMana - c, (cdSH, cdPO, 5)),
                        (bHP, bAtt), flag)
  where c = cost sp

getFlag :: GameState -> Flag
getFlag (_,_,f) = f

bossMove :: GameState -> GameState
bossMove ((pHP, pMana, (cdSH, cdPO, cdRE)), (bHP, bAtt), flag)
  | flag /= CONTINUE = ((pHP, pMana, (cdSH,cdPO,cdRE)), (bHP,bAtt), flag)
  | pHP' <= 0 = ((-1000, pMana, (0,0,0)), (bHP, bAtt), LOSE)
  | otherwise = ((pHP', pMana, (cdSH, cdPO, cdRE)), (bHP, bAtt), CONTINUE)
  where pHP' = pHP - (if' (cdSH > 0) (max (bAtt - 7) 1) bAtt)

castable :: GameState -> [Spell]
castable ((_,pMana,(cdSH, cdPO, cdRE)),_,_) = (m ++ d ++ s ++ p ++ c)
  where m = if' (pMana >= 53)                   [MISSLE] []
        d = if' (pMana >= 73)                   [DRAIN]  []
        s = if' ((pMana >= 113) && (cdSH == 0)) [SHIELD] []
        p = if' ((pMana >= 173) && (cdPO == 0)) [POISON] []
        c = if' ((pMana >= 229) && (cdRE == 0)) [CHARGE] []
--------------------------------------------------------------------------------
click :: GameState -> Spell -> GameState
click gs sp = bossMove $ applyEffects $ castSpell (applyEffects gs) sp

gameCycle :: (GameState, Integer) -> [(GameState, Integer)]
gameCycle (gs, spent)
  | f == WIN  = [(gs, spent)]
  | f == LOSE = []
  | otherwise = filter (\(gg,_) -> getFlag gg /= LOSE) $ next
  where f = getFlag gs
        av = castable $ applyEffects gs -- [Spell]
        next = map (\s -> (click gs s, spent + cost s)) av

playGame :: [(GameState, Integer)] -> [(GameState, Integer)]
playGame xs
  | elem CONTINUE flags = playGame next
  | otherwise           = xs
  where flags = map (\(gs,_) -> getFlag gs) xs
        next = nub $ concat $ map (gameCycle) xs
--------------------------------------------------------------------------------
drain :: GameState -> GameState
drain ((pHP, pMana, buffs), boss, flag) = ((pHP - 1, pMana, buffs), boss, flag')
  where flag' = if' (pHP <= 1) LOSE CONTINUE

hard :: GameState -> Spell -> GameState
hard gs sp = bossMove $ applyEffects $ castSpell (applyEffects ds) sp
  where ds = drain gs

gameCycleHard :: (GameState, Integer) -> [(GameState, Integer)]
gameCycleHard (gs, spent)
  | f == WIN  = [(gs, spent)]
  | f == LOSE = []
  | otherwise = filter (\(gg,_) -> getFlag gg /= LOSE) $ next
  where f = getFlag gs
        av = castable $ applyEffects gs -- [Spell]
        next = map (\s -> (hard gs s, spent + cost s)) av

playGameHard :: [(GameState, Integer)] -> [(GameState, Integer)]
playGameHard xs
  | elem CONTINUE flags = playGameHard next
  | otherwise           = xs
  where flags = map (\(gs,_) -> getFlag gs) xs
        next = nub $ concat $ map (gameCycleHard) xs
--------------------------------------------------------------------------------
main = do
  let player = (50,500,(0,0,0))
  let boss = (71,10)

  let k = playGame [((player, boss, CONTINUE), 0)]
  let winning = filter (\(gs,_) -> getFlag gs == WIN) k 
  let manaCost = map (snd) winning
  putStr "Part 1: "
  putStrLn $ show $ minimum manaCost

  let k = playGameHard [((player, boss, CONTINUE), 0)]
  let winning = filter (\(gs,_) -> getFlag gs == WIN) k
  let manaCost = map (snd) winning
  putStr "Part 2: "
  putStrLn $ show $ minimum manaCost
