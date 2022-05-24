{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Utils
    (
    withColFromTo
    ,withColFromToPixel
    ,polarCoordToCartesian
    ,degreesToRadians
    ,radiansToDegree
    ,commandsToSVG
    ,vX
    ,vY
    ,indexOfMaximum
    ,indexOfMinimum
    ,showCharArray
    ,oNewWithCam
    ,oNewWithSvgPosAndCam
    ) where

import Codec.Picture --(PixelRGBA8)
import Control.Lens
import qualified Data.List as DL
import Graphics.SvgTree
import Linear.V2
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene
import Reanimate.Svg
import Reanimate.ColorComponents
import qualified Data.Text as L

import Common.Others
--Color definition utility functions---------------------------------------------------------------------------------
withColFromTo :: String -> Int -> Int -> SVG -> SVG
withColFromTo color start end = withSubglyphs [start .. end] (\v -> withFillColor color v)
    
withColFromToPixel :: PixelRGBA8 -> Int -> Int -> SVG -> SVG
withColFromToPixel color start end = withSubglyphs [start .. end] (\v -> withFillColorPixel color v)
--------------------------------------------------------------------------------------------------------


      
--Utils--------------------------------------------------------------------------------------------------------------
                              
-- Transform polar coordinates to Cartesian coordinates
polarCoordToCartesian :: Floating a => a -> a -> V2 a
polarCoordToCartesian r ang = V2 x y
  where
    x = r * cos( degreesToRadians ang )
    y = r * sin( degreesToRadians ang )
    
    
degreesToRadians :: Floating a => a -> a
degreesToRadians deg = deg * pi / 180


radiansToDegree :: Floating a => a -> a
radiansToDegree rad = rad/(2 * pi) * 360

commandsToSVG :: [PathCommand] -> SVG
commandsToSVG commands= withStrokeColor "black" $ withFillOpacity 0 $ withStrokeWidth 0.05 $  mkPath commands

vX :: V2 a -> a
vX (V2 x _) = x

vY :: V2 a -> a
vY (V2 _ y) = y

indexOfMaximum :: [Integer] -> Int
indexOfMaximum xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

indexOfMinimum :: [Integer] -> Int
indexOfMinimum xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

showCharArray :: [Char] -> String
showCharArray arr = "[" ++ (DL.intersperse ',' arr) ++ "]"
---------------------------------------------------------------

-- Camera utils -------------------------------------------------------------------

oNewWithCam :: Renderable a => a -> Object s Camera -> Scene s (Object s a) 
oNewWithCam a cam = do 
                    res <- oNew a
                    cameraAttach cam res
                    return res
                    
oNewWithSvgPosAndCam :: SVG -> Object s Camera -> Scene s (Object s SVG) 
oNewWithSvgPosAndCam svg cam = do 
                                res <- oNewWithSvgPos svg
                                cameraAttach cam res
                                return res

----------------------------------------------------------------------------------
