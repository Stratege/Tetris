{-# LANGUAGE OverloadedStrings, BangPatterns, GeneralizedNewtypeDeriving #-}

import SDL
import Data.Text
import Linear.Affine
import Linear.V2
import Linear.V4
import Foreign.C.Types
import Data.Maybe
import qualified Data.List as L
import Data.Word
import System.IO.Unsafe
import System.Random
import Data.Time.Clock

newtype Score = Score Int deriving (Num,Show)
data Level = Level Int deriving Show
data Map = Map Int Int [[Field]] deriving Show
data Field = Empty | Filled Color deriving Show
data Color = Teal | Blue | Orange | Yellow | Green | Violet | Red deriving Show
data Tetris = Tetris Map (Maybe FallingElement) Score Level deriving Show
type Palett = (V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8)

data Shape = Shape Color [[Bool]] deriving Show
data FallingElement = FallingElement Shape (V2 Int) deriving Show -- top left pos
data Direction = Down | Left | Right | Rotate deriving (Eq,Show)

palette :: Palett
palette = (black,teal,blue,orange,yellow,green,violet,red)
   where black = V4 0 0 0 0
         teal = V4 0 255 255 0
         blue = V4 0 0 255 0
         orange = V4 255 127 0 0
         yellow = V4 255 255 0 0 
         green = V4 0 255 0 0
         violet = V4 127 0 255 0
         red = V4 255 0 0 0

fieldToDrawColor :: Field -> Palett -> V4 Word8
fieldToDrawColor Empty           (a,_,_,_,_,_,_,_) = a
fieldToDrawColor (Filled Teal)   (_,b,_,_,_,_,_,_) = b 
fieldToDrawColor (Filled Blue)   (_,_,c,_,_,_,_,_) = c
fieldToDrawColor (Filled Orange) (_,_,_,d,_,_,_,_) = d
fieldToDrawColor (Filled Yellow) (_,_,_,_,e,_,_,_) = e
fieldToDrawColor (Filled Green)  (_,_,_,_,_,f,_,_) = f
fieldToDrawColor (Filled Violet) (_,_,_,_,_,_,g,_) = g
fieldToDrawColor (Filled Red)    (_,_,_,_,_,_,_,h) = h

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

emptyMap = Map 10 40 (L.replicate 40 (L.replicate 10 Empty))

emptyTetris = Tetris emptyMap Nothing (Score 0) (Level 0)

-- note createRGBSurface explicitly permits palettes, this might make things simpler + more efficient, potentially?
--testPalette = ((V4 0 0 0 0),(V4 255 0 0 0),(V4 0 255 0 0),(V4 0 0 255 0))
testMap = Map 10 40 (rep 20 (rep 10 Empty) ++ rep 8 (rep 4 Empty ++ [Filled Green,Filled Green] ++ rep 4 Empty) ++ rep 8 (rep 1 Empty ++ rep 8 (Filled Red) ++ rep 1 Empty) ++ rep 4 (rep 2 Empty ++ rep 6 (Filled Blue) ++ rep 2 Empty))
   where rep = L.replicate
testTetris = Tetris testMap Nothing (Score 0) (Level 0)

getMap (Tetris map _ _ _) = map

getFieldRect (totalFieldWidthCount,totalFieldHeightCount) (curX,curY) (Rectangle (P (V2 left top)) (V2 width height)) = Rectangle (P (V2 new_left new_top)) (V2 new_width new_height)
    where tfwc = realToFrac totalFieldWidthCount
          tfhc = realToFrac totalFieldHeightCount
          frac_x = realToFrac curX / tfwc
          frac_y = realToFrac curY / tfhc
          fieldsize_x = 1 / tfwc
          fieldsize_y = 1 / tfhc
          new_left = left + floor (frac_x * realToFrac width)
          new_top = top + floor (frac_y * realToFrac height)
          new_width = floor (fieldsize_x * realToFrac width)
          new_height = floor (fieldsize_y * realToFrac height)

drawFields (Map x y lls) palett sur = do
    size <- surfaceDimensions sur
    let lls' =  fmap (L.zipWith (\x (a,b) -> (x,a,b)) [0..]) $ L.zipWith (\x y -> fmap (\a -> (x,a)) y) [0..] lls
    let lls'' = fmap (fmap (drawField x y palett size sur)) lls'
    sequence_ . fmap sequence $ lls''

drawField :: Int -> Int -> Palett -> (V2 CInt) -> Surface -> (Int,Int,Field) -> IO ()
drawField x y palett size sur (curX,curY,field) = surfaceFillRect sur (Just drawRect) drawColor
    where drawRect = getFieldRect (x,y) (curX,curY) (Rectangle (P (V2 0 0)) size)
          drawColor = fieldToDrawColor field palett

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

moveFalling :: Direction -> Map -> FallingElement -> (Map,Either Score FallingElement)
moveFalling dir map@(Map x y lls) fe@(FallingElement shape (V2 x2 y2))
   | x2 < 0 || x <= x3 = trace' (pack ("boom? " ++ show x ++ " - " ++ show x3)) (map,Prelude.Right fe)
   | y <= (y3+1) || collides ((\(Shape _ x) -> x) newShape) nextFields = (if y2 == 0 then trace' "you lost" else id) (doScoring map)
   | otherwise = (addFields fe2 mapWithoutFalling,Prelude.Right fe2)
   where (V2 x3 y3) = (V2 x2 y2) + getShapeOffsets newShape
         mapWithoutFalling = removeFields fe map
         nextFields = getFields (Rectangle (P nextPos) (V2 4 4)) mapWithoutFalling
         fe2 = (FallingElement newShape nextPos)
         nextPos
             | dir == Down = V2 x2 (y2+1)
             | dir == Main.Left = V2 (if x2 >0 then x2-1 else x2) y2
             | dir == Main.Right = V2 (if x3 < (x-1) then x2+1 else x2) y2
             | dir == Rotate = V2 x2 y2
         newShape
             | dir == Rotate = (\(Shape a x) -> (Shape a (rotate x))) shape
             | otherwise = shape

doScoring (Map x y lls) = (Map x y lls'',Prelude.Left (Score linesResolved))
   where lls' = L.filter (not . L.all isFilled) lls
         linesResolved = y - L.length lls'
         lls'' = L.replicate linesResolved (L.replicate x Empty) ++ lls'

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

main = do 
    (w,sur) <- initStuff
    windowFormat <- surfaceFormat sur
--    sur2 <- loadMediaOptimized windowFormat
    size <- surfaceDimensions sur
--    surfaceBlitScaled sur2 Nothing sur Nothing
    updateWindowSurface w
--    r <- getRendererInfo =<< createRenderer w 0 defaultRenderer
--    showSimpleMessageBox (Just w) Error "title" (pack . Prelude.unlines . partitionAt 50 . show $ r)
    simpleLoop w emptyTetris palette sur 0
    destroyWindow w
    quit

--todo: exception handling
initStuff = do
    initializeAll
    w <- createWindow "Tetris" (defaultWindow {windowInitialSize = V2 200 800})
    s <- getWindowSurface w
    return (w,s)

loadMediaOptimized format = do
    b <- loadBMP "ar.bmp"
    x <- convertSurface b format
    freeSurface b
    return x

toBool 0 = False
toBool x = True

insertNewlines [] = []
insertNewlines xs = L.take 80 xs ++ insertNewlines' (L.drop 80 xs)

insertNewlines' [] = []
insertNewlines' xs = '\n' : L.take 80 xs ++ insertNewlines' (L.drop 80 xs)

nextGameStep x Nothing = return x
nextGameStep (Tetris map@(Map x _ _) Nothing score level) _ = do
    i <- randomRIO (0,L.length blocks - 1)
    let newElm = FallingElement (blocks L.!! i) (V2 (x `div` 2) 0) 
    return $ Tetris (addFields newElm map) (Just newElm) score level
nextGameStep (Tetris map (Just elm) score level) (Just dir) = return $ Tetris map' elm'' score' level
    where (map',elm') = moveFalling dir map elm
          f (Prelude.Left score') = (score'+score,Nothing)
          f (Prelude.Right elm'') = (score,Just elm'')
          (score',elm'') = f elm'

isKeyboardEvent (KeyboardEvent _) = True
isKeyboardEvent _ = False

traceIO' x = showSimpleMessageBox Nothing Information "gamestate" (pack . insertNewlines . show $ x)
trace' x y = unsafePerformIO (traceIO' x) `seq` y

parseKeyboardEvent (KeyboardEvent (KeyboardEventData _ Pressed repeat (Keysym _ c _)))
    | c == KeycodeLeft = Just Main.Left
    | c == KeycodeRight = Just Main.Right
    | c == KeycodeDown = Just Down
    | c == KeycodeR = Just Rotate
    

parseKeyboardEvent _ = Nothing

simpleLoop !w tetris palette sur deltaT = do
    startT <- getCurrentTime
    e <- pollEvents
    let !x = Prelude.length $ Prelude.filter ((== (WindowClosedEvent (WindowClosedEventData w))) . eventPayload) e
    let !y = Prelude.filter isKeyboardEvent . fmap eventPayload $ e
    if x == 0 then do
        tetris' <- if L.length y == 0 then return tetris else (nextGameStep tetris (parseKeyboardEvent . L.head $ y))
        (deltaT',tetris'') <- advanceByTime deltaT tetris'
        drawFields (getMap tetris'') palette sur 
        updateWindowSurface w
        delay 10
        finT <- getCurrentTime
        let deltaT'' = deltaT' + diffUTCTime finT startT 
        simpleLoop w tetris'' palette sur deltaT''
    else return ()

advanceByTime deltaT tetris = do
        let (deltaT',b) = if (deltaT > timeBetweenDrops) then (deltaT - timeBetweenDrops,True) else (deltaT,False)
        if not b then return (deltaT',tetris) else (nextGameStep tetris (Just Down) >>= advanceByTime deltaT')
        where timeBetweenDrops = 0.5
