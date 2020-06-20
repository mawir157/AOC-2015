type Player = (Integer, Integer, Integer) -- Hp Att Def
type Item = (Integer, Integer, Integer) -- Cost Att Def

weapons = [(8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0)]
armour  = [(0,0,0),(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,4)]
rings   = [(0,0,0),(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3)]

gear = [ (w,a,r1,r2) | w <- weapons, a <- armour, r1 <- rings,
                       r2 <- rings, r1 /= r2]

cost :: (Item,Item,Item,Item) -> Integer
cost ((c1,_,_),(c2,_,_),(c3,_,_),(c4,_,_)) = c1 + c2 + c3 + c4

equip :: Player -> (Item,Item,Item,Item) -> Player
equip (pHP,pAtt,pDef) ((_,a1,d1),(_,a2,d2),(_,a3,d3),(_,a4,d4)) =
  (pHP, pAtt + a1 + a2 + a3 + a4, pDef + d1 + d2 + d3 + d4)

win :: Player ->  Player -> Bool
win p1 p2 = pHP /= -1
  where ((pHP,_,_),(bHP,_,_)) = battle (p1, p2)

battle :: (Player, Player) -> (Player, Player)
battle ((pHP,pAtt,pDef),(bHP,bAtt,bDef))
  | bHP' <= 0 = ((pHP,pAtt,pDef),(-1,-1,-1))
  | pHP' <= 0 = ((-1,-1,-1),(bHP',bAtt',bDef'))
  | otherwise = battle ((pHP',pAtt',pDef'),(bHP',bAtt',bDef'))
  where (bHP',bAtt',bDef') = (bHP - (max (pAtt - bDef) 1), bAtt, bDef) 
        (pHP',pAtt',pDef') = (pHP - (max (bAtt - pDef) 1), pAtt, pDef)

main = do
  let p1 = (100,0,0) :: Player
  let boss = (104, 8, 1) :: Player
  let goodGear = filter (\x -> cost x <= 100) gear

  let s = map (\g -> (win (equip p1 g) boss, cost g)) goodGear
  let s' = minimum $ map (snd) $ filter (fst) s
  putStr "Part 1: "
  putStrLn $ show s'

  let s = map (\g -> (win (equip p1 g) boss, cost g)) gear
  let s' = maximum $ map (snd) $ filter (not . fst) s
  putStr "Part 2: "
  putStrLn $ show s'
