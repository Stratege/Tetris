{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Game where

import SDL
import Data.Text
import Linear.Affine
import Linear.V2
import Linear.V4
import qualified Data.List as L
import System.IO.Unsafe
import System.Random


newtype Score = Score Int deriving (Num,Show)
newtype Level = Level Int deriving (Num,Show)
data Map = Map Int Int [[Field]] deriving Show
data Field = Empty | Filled Color deriving Show
data Color = Teal | Blue | Orange | Yellow | Green | Violet | Red | Black deriving Show
data Tetris = Tetris Map (Maybe FallingElement) Score Level deriving Show

data Shape = Shape Color [[Bool]] deriving Show
data FallingElement = FallingElement Shape (V2 Int) deriving Show -- top left pos
data Direction = Down | Left | Right | Rotate deriving (Eq,Show)

data RawParticle = RawParticle Color (V2 Int)

blocks = [line,revL,revR,block,sblock,tblock,zblock]
   where line = Shape Teal (f $ rep 4 [1,0,0,0])
         revL = Shape Blue (f $ [1,0,0,0] : [1,1,1,0] : rep 2 (rep 4 0))
         revR = Shape Orange (f $ [0,0,1,0] : [1,1,1,0] : rep 2 (rep 4 0))
         block = Shape Yellow (f $ rep 2 [1,1,0,0] ++ rep 2 (rep 4 0))
         sblock = Shape Green (f $ [0,1,1,0] : [1,1,0,0] : rep 2 (rep 4 0))
         tblock = Shape Violet (f $ [0,1,0,0] : [1,1,1,0] : rep 2 (rep 4 0))
         zblock = Shape Red (f $ [1,1,0,0] : [0,1,1,0] : rep 2 (rep 4 0))
         f :: [[Int]] -> [[Bool]]
         f = fmap (fmap toBool)
         rep = L.replicate

emptyMap = Map x y (L.replicate y (L.replicate x Empty))
   where x = 10
         y = 30

emptyTetris = Tetris emptyMap Nothing (Score 0) (Level 0)

testMap = Map 10 40 (rep 20 (rep 10 Empty) ++ rep 8 (rep 4 Empty ++ [Filled Green,Filled Green] ++ rep 4 Empty) ++ rep 8 (rep 1 Empty ++ rep 8 (Filled Red) ++ rep 1 Empty) ++ rep 4 (rep 2 Empty ++ rep 6 (Filled Blue) ++ rep 2 Empty))
   where rep = L.replicate
testTetris = Tetris testMap Nothing (Score 0) (Level 0)

getMap (Tetris map _ _ _) = map
getLevel (Tetris _ _ _ level) = level

unScore (Score x) = x
unLevel (Level x) = x

rotate :: [[Bool]] -> [[Bool]]
rotate xss = (++ L.replicate y (L.replicate (L.length xss) False)) . fmap (\xs -> L.drop x xs ++ L.replicate x False) . L.drop y $ xss'
   where (V2 x y) = getShapeStart (Shape undefined xss')
         xss' = rotate' xss

rotate' ([]:_) = []
rotate' xss = L.reverse (fmap L.head xss) : rotate' (fmap L.tail xss)


getShapeOffsets (Shape _ lls) =  getShapeOffsetHelper lls max 0

getShapeStart (Shape _ lls) = getShapeOffsetHelper lls min (L.length lls + 1)

getShapeOffsetHelper :: [[Bool]] -> (Int -> Int -> Int) -> Int -> (V2 Int)
getShapeOffsetHelper lls g v = (V2 x y)
   where x = f lls
         y = f $ L.transpose lls
         f = L.foldr g v . fmap (L.foldr g v . fmap fst . L.filter (\(_,b) -> b) . L.zipWith (,) [0..])

moveFalling :: Direction -> Map -> FallingElement -> (Map,Either Score FallingElement,[RawParticle])
moveFalling dir map@(Map x y lls) fe@(FallingElement shape (V2 x2 y2))
   | x2 < 0 || x <= x3 = (map,Prelude.Right fe,[])
   | (dir == Down) && (y <= (y3+1) || collides ((\(Shape _ x) -> x) newShape) nextFields) = (if y2 == 0 then trace' "you lost" else id) (doScoring map)
   | (dir /= Down) && collides ((\(Shape _ x) -> x) newShape) nextFields = (map,Prelude.Right fe,[])
   | otherwise = (addFields fe2 mapWithoutFalling,Prelude.Right fe2,[])
   where (V2 x3 y3) = (V2 x2 y2) + getShapeOffsets newShape
         mapWithoutFalling = removeFields fe map
         nextFields = getFields (Rectangle (P nextPos) (V2 4 4)) mapWithoutFalling
         fe2 = (FallingElement newShape nextPos)
         nextPos
             | dir == Down = V2 x2 (y2+1)
             | dir == Game.Left = V2 (if x2 >0 then x2-1 else x2) y2
             | dir == Game.Right = V2 (if x3 < (x-1) then x2+1 else x2) y2
             | dir == Rotate = V2 x2 y2
         newShape
             | dir == Rotate = (\(Shape a x) -> (Shape a (rotate x))) shape
             | otherwise = shape

getColor Empty = Black
getColor (Filled x) = x

doScoring (Map x y lls) = (Map x y lls'',Prelude.Left (Score linesResolved),rawParticles)
   where (lls',rawParticles) = fst . L.foldl (\((xs,p),n) x -> (if (L.all isFilled x) then (xs,L.zipWith (\a b -> RawParticle b (V2 a n)) [0..] (L.map getColor x) ++ p) else (x:xs,p),n+1)) (([],[]),0) $ lls
         linesResolved = y - L.length lls'
         lls'' = L.replicate linesResolved (L.replicate x Empty) ++ L.reverse lls'

getFields :: Rectangle Int -> Map -> [[Field]]
getFields (Rectangle _ (V2 0 _)) _ = []
getFields (Rectangle _ (V2 _ 0)) _ = []
getFields (Rectangle (P (V2 x y)) (V2 w h)) (Map x2 y2 lls)
    | x >= x2 = []
    | y >= y2 = []
    | otherwise = getFieldLine : getFields (Rectangle (P (V2 x (y+1))) (V2 w (h-1))) (Map x2 y2 lls)
    where getFieldLine = L.take w . L.drop x $ (lls L.!! y)

setFields pos fields (Map a b lss) = (Map a b (setFieldsHelper pos fields lss))

setFieldsHelper :: (V2 Int) -> [[Field]] -> [[Field]] -> [[Field]]
setFieldsHelper _ [] lss = lss
setFieldsHelper _ _ [] = []
setFieldsHelper (V2 x y) a@(xs:xss) (ls:lss)
   | y > 0 = ls : setFieldsHelper (V2 x (y-1)) a lss
   | otherwise = (L.take x ls ++ L.take x' xs ++ (L.drop x' ls)) : setFieldsHelper (V2 x y) xss lss
   where x' = min (L.length ls) (x+L.length xs)

isFilled Empty = False
isFilled _ = True

collides :: [[Bool]] -> [[Field]] -> Bool
collides xss = L.any id . L.concat . L.zipWith (L.zipWith (\a b -> a && isFilled b)) xss

removeFields :: FallingElement -> Map -> Map
removeFields (FallingElement (Shape _ bss) pos) = modFields (\a b -> if a then Empty else b) bss pos

addFields :: FallingElement -> Map -> Map
addFields (FallingElement (Shape col bss) pos) = modFields (\a b -> if a then Filled col else b) bss pos

modFields f bss pos map = setFields pos newFields map
    where deltaFields = getFields deltaRect map
          deltaRect = (Rectangle (P pos) (V2 (L.length bss) (L.length . L.head $ bss)))
          newFields = L.zipWith (L.zipWith f) bss deltaFields 

partitionAt n xs = partitionAt' n n xs []
    where partitionAt' n m [] acc = [Prelude.reverse acc]
          partitionAt' n 0 xs acc = (Prelude.reverse acc) : partitionAt' n n xs []
          partitionAt' n m (x:xs) acc = partitionAt' n (m-1) xs (x:acc)

nextGameStep x Nothing = return (x,[])
nextGameStep (Tetris map@(Map x _ _) Nothing score level) _ = do
    i <- randomRIO (0,L.length blocks - 1)
    let newElm = FallingElement (blocks L.!! i) (V2 (x `div` 2) 0) 
    return $ (Tetris (addFields newElm map) (Just newElm) score level,[])
nextGameStep (Tetris map (Just elm) score level) (Just dir) = return $ (Tetris map' elm'' score' (g level),rawParticles)
    where (map',elm',rawParticles) = moveFalling dir map elm
          f (Prelude.Left score') = (score'+score,Nothing)
          f (Prelude.Right elm'') = (score,Just elm'')
          (score',elm'') = f elm'
          g x = if unScore score' > unLevel ((x*x+1)*10) then g (x+1) else x

toBool 0 = False
toBool x = True

advanceByTime deltaT tetris = advanceByTime' deltaT (tetris,[])

advanceByTime' deltaT (tetris,rawParticles) = do
        let (deltaT',b) = if (deltaT > timeBetweenDrops) then (deltaT - timeBetweenDrops,True) else (deltaT,False)
        if not b then return (deltaT',tetris,rawParticles) else (nextGameStep tetris (Just Down) >>= advanceByTime' deltaT')
        where timeBetweenDrops = max 0.1 (0.5 - 0.05 * (realToFrac . unLevel . getLevel $ tetris))



-- todo
traceIO' x = showSimpleMessageBox Nothing Information "gamestate" (pack . insertNewlines . show $ x)
trace' x y = unsafePerformIO (traceIO' x) `seq` y

insertNewlines [] = []
insertNewlines xs = L.take 80 xs ++ insertNewlines' (L.drop 80 xs)

insertNewlines' [] = []
insertNewlines' xs = '\n' : L.take 80 xs ++ insertNewlines' (L.drop 80 xs)