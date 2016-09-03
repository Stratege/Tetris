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
import Game

type Palett = (V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8,V4 Word8)

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

-- note createRGBSurface explicitly permits palettes, this might make things simpler + more efficient, potentially?
--testPalette = ((V4 0 0 0 0),(V4 255 0 0 0),(V4 0 255 0 0),(V4 0 0 255 0))

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

isKeyboardEvent (KeyboardEvent _) = True
isKeyboardEvent _ = False

parseKeyboardEvent (KeyboardEvent (KeyboardEventData _ Pressed repeat (Keysym _ c _)))
    | c == KeycodeLeft = Just Game.Left
    | c == KeycodeRight = Just Game.Right
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
