{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Others
    (
    drawSVGSequentially
    ,splitInitLast
    ,forkLag
    ,forkAllWithDifferentLags
    ,forkAllWithLag
    ,forkAll
    ,oTweenContext
    ,withColor
    ,withColorPixel
    ,oNewWithSvgPos
    ) where

import Codec.Picture --(PixelRGBA8)
import Control.Lens
import Graphics.SvgTree
import Linear.V2
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene
import Reanimate.Svg
import Reanimate.ColorComponents


--CODE FROM MPILGREM-------------------------------------------

drawSVGSequentially :: SVG -> Animation
drawSVGSequentially svgToDRaw = mkAnimation 2 animation
 where
  animation t = partialSvg (curveS 2 t) $ pathify $  svgToDRaw
  
---------------------------------------------------------------


--CODE FROM IZUZU--------------------------------------------

splitInitLast :: [a] -> Maybe ([a], a)
splitInitLast [] = Nothing
splitInitLast [x] = Just ([], x)
splitInitLast (x:xs) = Just (x:a, b)
    where Just (a,b) = splitInitLast xs
        
forkLag :: Double -> Scene s a -> Scene s ()
forkLag time action = fork action >> wait time

forkAllWithDifferentLags :: [Double] -> [Scene s a] -> Scene s ()
forkAllWithDifferentLags _ [] = pure ()
forkAllWithDifferentLags lags scenes =
    sequence_ (zipWith forkLag (lags ++ repeat 0) initScenes)
    >> lastScene
    >> pure ()
    where Just (initScenes, lastScene) = splitInitLast scenes


forkAllWithLag :: Double -> [Scene s a] -> Scene s ()
forkAllWithLag _ [] = pure ()
forkAllWithLag time scenes = forkAllWithDifferentLags (repeat time) scenes

forkAll :: [Scene s a] -> Scene s ()
forkAll = forkAllWithLag 0

oTweenContext :: Object s a -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
oTweenContext obj dur f = oTween obj dur $ \t -> oContext %~ (f t .)

withColor :: String -> SVG -> SVG
withColor color = withStrokeColor color . withFillColor color

withColorPixel :: PixelRGBA8 -> SVG -> SVG
withColorPixel color = withStrokeColorPixel color . withFillColorPixel color

oNewWithSvgPos:: SVG -> Scene s (Object s SVG)
oNewWithSvgPos svg = do
    let
        (minX, minY, w, h) = boundingBox svg
        cenX = minX + w/2
        cenY = minY + h/2
    obj <- oNew $ center svg
    oModify obj $ oTranslate .~ V2 cenX cenY
    pure obj

---------------------------------------------------------------------

revGlyphs svg = mkGroup [ ctx glyph | (ctx, _attr, glyph) <- reverse $ svgGlyphs svg ]

