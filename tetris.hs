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
fieldToDrawColor Empty = colorLookup Black
fieldToDrawColor (Filled x) = colorLookup x

colorLookup :: Color -> Palett -> V4 Word8
colorLookup Black  (a,_,_,_,_,_,_,_) = a
colorLookup Teal   (_,b,_,_,_,_,_,_) = b 
colorLookup Blue   (_,_,c,_,_,_,_,_) = c
colorLookup Orange (_,_,_,d,_,_,_,_) = d
colorLookup Yellow (_,_,_,_,e,_,_,_) = e
colorLookup Green  (_,_,_,_,_,f,_,_) = f
colorLookup Violet (_,_,_,_,_,_,g,_) = g
colorLookup Red    (_,_,_,_,_,_,_,h) = h

-- note createRGBSurface explicitly permits palettes, this might make things simpler + more efficient, potentially?
--testPalette = ((V4 0 0 0 0),(V4 255 0 0 0),(V4 0 255 0 0),(V4 0 0 255 0))

getFieldRect (totalFieldWidthCount,totalFieldHeightCount) (curX,curY) (Rectangle (P (V2 left top)) (V2 width height)) = Rectangle (P (V2 new_left new_top)) (V2 new_width new_height)
    where tfwc = realToFrac totalFieldWidthCount
          tfhc = realToFrac totalFieldHeightCount
          frac_x = realToFrac curX / tfwc
          frac_y = realToFrac curY / tfhc
          fieldsize_x = 1 / tfwc
          fieldsize_y = 1 / tfhc
          new_left = left + round (frac_x * realToFrac width)
          new_top = top + round (frac_y * realToFrac height)
          new_width = round (fieldsize_x * realToFrac width)
          new_height = round (fieldsize_y * realToFrac height)

drawFields (Map x y lls) palett ren surOffset surSize = do
    let lls' =  fmap (L.zipWith (\x (a,b) -> (x,a,b)) [0..]) $ L.zipWith (\x y -> fmap (\a -> (x,a)) y) [0..] lls
    let lls'' = fmap (fmap (drawField x y palett surOffset surSize ren)) lls'
    sequence_ . fmap sequence $ lls''

drawField :: Int -> Int -> Palett -> (V2 CInt) -> (V2 CInt) -> Renderer -> (Int,Int,Field) -> IO ()
drawField x y palett surOffset size ren (curX,curY,field) = rendererDrawColor ren $= drawColor >> fillRect ren (Just drawRect)
    where drawRect = getFieldRect (x,y) (curX,curY) (Rectangle (P surOffset) size)
          drawColor = fieldToDrawColor field palett

main = do 
    (w,ren) <- initStuff
    present ren
    simpleLoop w emptyTetris palette ren 0 []
    destroyWindow w
    quit

--todo: exception handling
initStuff = do
    initializeAll
    w <- createWindow "Tetris" (defaultWindow {windowInitialSize = V2 380 900})
    r <- createRenderer w 0 defaultRenderer
    return (w,r)

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

--simpleLoop :: _ -> Tetris -> Palett -> Renderer -> _ -> [Particle] -> IO ()
simpleLoop !w tetris palette ren deltaT particles = do
    startT <- getCurrentTime
    e <- pollEvents
    let !x = Prelude.length $ Prelude.filter ((== (WindowClosedEvent (WindowClosedEventData w))) . eventPayload) e
    let !y = Prelude.filter isKeyboardEvent . fmap eventPayload $ e
    if x == 0 then do
        (tetris',rawParticles) <- if L.length y == 0 then return (tetris,[]) else (nextGameStep tetris (parseKeyboardEvent . L.head $ y))
        (deltaT',tetris'',rawParticles2) <- advanceByTime deltaT tetris'
        particles2 <- convertParticles tetris ren (rawParticles ++ rawParticles2)
        let particles' = computeParticles (particles2++particles)
        draw tetris'' palette ren
        drawParticles ren palette particles'
        present ren
        delay 10
        finT <- getCurrentTime
        let deltaT'' = deltaT' + diffUTCTime finT startT 
        simpleLoop w tetris'' palette ren deltaT'' particles'
    else return ()


--todo: figure out good shrinking-to-size algo
draw tetris palette ren = do
    (surOffset,sizeField) <- calculateGameArea tetris ren
    rendererDrawColor ren $= (V4 255 0 255 0)
    clear ren
    drawFields (getMap tetris) palette ren surOffset sizeField

calculateGameArea tetris ren = do
    (Just (Rectangle _ (V2 w h))) <- get . rendererViewport $ ren
    let w' = w - leftWidth
    let sizeField = (V2 w' (floor (realToFrac w' / aspect)))
    let surOffset = (V2 leftWidth 0)
    return (surOffset,sizeField)
    where (Map x y _) = getMap tetris
          aspect = realToFrac x / realToFrac y
          leftWidth = 80


data Particle = Particle Color (V2 Int) (Phase,Int)
data Phase = Growing | Decaying

convertParticles tetris sur rawParticles = do
    (surOffset,sizeField) <- calculateGameArea tetris sur
    mapM (convertParticle (\x2 y2 -> getFieldRect (x,y) (fromIntegral x2,fromIntegral y2) (Rectangle (P surOffset) sizeField))) rawParticles
    where (Map x y _) = getMap tetris

convertParticle f (RawParticle c (V2 x2 y2)) = do
    let (Rectangle (P (V2 x y)) (V2 w h)) = f x2 y2
    x3 <- randomRIO (x,x+w)
    y3 <- randomRIO (y,y+h)
    return (Particle c (V2 (fromIntegral x3) (fromIntegral y3)) (Growing,0))

drawParticles ren palett xs = do
     mode <- get (rendererDrawBlendMode ren)
     rendererDrawBlendMode ren $= BlendAdditive
     drawParticles' ren palett xs
     rendererDrawBlendMode ren $= mode

drawParticles' ren palett [] = return ()
drawParticles' ren palett ((Particle c (V2 x y) (phase,n)):xs) = q >> drawParticles' ren palett xs
    where drawRect = Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) particleSize
          drawColor = addAlpha (25*n) . whiten 100 . colorLookup c $ palett
          n' = 5 + fromIntegral (n `div` 2)
          particleSize = (V2 n' n')
          q = rendererDrawColor ren $= drawColor >> fillRect ren (Just drawRect)

addAlpha :: Int -> V4 Word8 -> V4 Word8
addAlpha n (V4 x y z a) = (V4 x y z (f a))
   where f b = fromIntegral $ min 255 (n + fromIntegral b)

whiten :: Int -> V4 Word8 -> V4 Word8
whiten n (V4 x y z a) = (V4 (f x) (f y) (f z) a)
    where f b = fromIntegral $ min 255 (n + fromIntegral b)

   
computeParticles [] = []
computeParticles ((Particle c (V2 x y) p):xs) = maybe xs' (\p' -> (Particle c (V2 x y) p'):xs') p2
    where f (Growing,n) = if n >= framesPerParticlePhase then Just (Decaying,n) else Just (Growing,n+1)
          f (Decaying,n) = if n == 0 then Nothing else Just (Decaying,n-1)
          p2 = f p
          xs' = computeParticles xs
          framesPerParticlePhase = 20