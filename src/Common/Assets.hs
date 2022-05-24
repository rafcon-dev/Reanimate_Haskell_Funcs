{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Assets
    (
    calligraCfg
    ,bgPixelColor
    ,funcColorPixel
    ,tokensColorPixel
    ,textColorPixel
    ,blueColorPixel
    ,fadeOverlay
    ,bg
    ,numberRoll
    ,fullArrow
    ,dynamicArrow
    ,get1ParFunctionSignature
    ,get1ParExpandedFunction
    ,getFunctionAtEnd
    ,counter
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

import qualified Data.Text as L
import qualified Data.List as DL

import Common.Utils

--Latex Text Config
calligraCfg :: TexConfig
calligraCfg = TexConfig LaTeX
  ["\\usepackage{calligra}"
  ,"\\renewcommand*\\familydefault{\\ttdefault}"
  ,"\\usepackage[T1]{fontenc}"]
  ["\\normalfont"]
  
  --COLOR PALETTE----------------------------------------------------------
bgPixelColor = PixelRGBA8 44 50 80 255 --dark blue
funcColorPixel = PixelRGBA8 247 156 150 255 --ligh red
tokensColorPixel = PixelRGBA8 255 244 184 255 --light yellow
textColorPixel = PixelRGBA8 229 232 245 255 --off white
blueColorPixel = PixelRGBA8 148 203 236 255 -- light blue
-------------------------------------------------------------------------

--SVG assets
--overlay to apply a fadeIn Effect in the beginning and end of the animation
fadeOverlay = mkBackground "black"

--background plane
bg = mkBackgroundPixel bgPixelColor

--Standard SVG generators------------------------------------------------------------
get1ParFunctionSignature :: String -> String -> String -> Tree
get1ParFunctionSignature funcNameStr par1NameStr par2NameStr = 
    withColFromToPixel funcColorPixel 0 funcEnd $
    withColFromToPixel funcColorPixel colonsStart colonsEnd $
    withColFromToPixel tokensColorPixel par1Start par1End $
    withColFromToPixel funcColorPixel arrow arrow $
    withColFromToPixel tokensColorPixel par2Start par2End $
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
    where
        funcEnd = length funcNameStr - 1
        colonsStart = funcEnd + 1
        colonsEnd = colonsStart + 1
        par1Start = colonsEnd + 1
        par1End = par1Start -1 + length par1NameStr
        arrow = par1End + 1
        par2Start = arrow + 1
        par2End = par2Start -1 + length par2NameStr
        txt = funcNameStr ++ " :: " ++ par1NameStr ++ " $\\rightarrow$ " ++ par2NameStr
        
get1ParExpandedFunction :: String -> String -> String -> Tree
get1ParExpandedFunction funcNameStr inputLabelStr inputStr = 
    withColFromToPixel funcColorPixel 0 funcEnd $ 
    withColFromToPixel textColorPixel input1Start input1End $ 
    withColFromToPixel tokensColorPixel equalPos equalPos $ 
    withColFromToPixel funcColorPixel input2Start input2End $ 
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg txt
    where
        funcEnd = length funcNameStr - 1
        input1Start = funcEnd + 1
        input1End = input1Start - 1 + length inputLabelStr
        equalPos = funcEnd + length inputLabelStr + 1
        input2Start = equalPos + 1
        input2End = input2Start - 1 + length funcNameStr
        txt = L.pack $ funcNameStr ++ " " ++ inputLabelStr ++ " =" ++ " " ++ funcNameStr ++ " " ++ inputStr
        
getFunctionAtEnd :: String -> String -> String -> Tree
getFunctionAtEnd funcNameStr inputLabelStr resultStr = 
    withColFromToPixel funcColorPixel 0 funcEnd $ 
    withColFromToPixel textColorPixel input1Start input1End $ 
    withColFromToPixel tokensColorPixel equalPos equalPos $ 
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg txt
    where
        funcEnd = length funcNameStr - 1
        input1Start = funcEnd + 1
        input1End = input1Start - 1 + length inputLabelStr
        equalPos = funcEnd + length inputLabelStr + 1
        txt = L.pack $ funcNameStr ++ " " ++ inputLabelStr ++ " =" ++ " " ++ resultStr
----------------------------------------------------------------------------------------------------

--to create a animated sprite that simulates a number roll effect (like a slot machine)
--basically it's all the numbers spaced vertically, with a mask so only one number is shown
--if the numbers are translated but the mask isn't, the masks works like a slot machine window
numberRoll ::  Integer -> Double -> SVG
numberRoll initialIndex t = mkGroup [
    mkClipPath "maskLabel" $ removeGroups mask,
    withClipPathRef (Ref "maskLabel") $ withFillColorPixel textColorPixel $ translate 0 yFinal numbersOfTheRollVertical
    ]
    where
    yFinal = -(t) - ((fromIntegral initialIndex)*rollSpacing)
    mask = center $ mkRect 4 1
    numbersOfTheRollVertical = mkGroup $ map (\i -> translate 0 (i*rollSpacing) $ center $ latexCfg calligraCfg $ L.pack (show $ round i)) [-50 .. 50] --higher values are not performant, this should be optimized somehow
    rollSpacing = 1

    --simple arrow, composed of a line and a triangle on the end
fullArrow :: Double -> SVG
fullArrow len = mkGroup[mkPath [MoveTo OriginAbsolute [V2 0 0], LineTo OriginAbsolute [V2 0 (len)]], translate 0 len $ rotate (0) arrowPoint]
    where        
          arrowPoint = mkPath $ [MoveTo OriginAbsolute [V2 0 0],
            LineTo OriginAbsolute [V2 0.15 0],
            LineTo OriginAbsolute [V2 0 0.2],
            LineTo OriginAbsolute [V2 (-0.15) 0],
            EndPath
            ]

--similarly to the numberRoll effect, to show a growing and shrinking arrow, we animate the mask
--negative time values are used to make the arrow grow, while positive time values make the arrow shrink
--when the arrow grows, we want to animate the arrow (going into the mask)
--however, when the arrow shrinks, we want to animate the mask going along the arrow
--that's why both parts have variables assigned to their translation that depend on time, for each occasion

dynamicArrow ::  PixelRGBA8 -> Double -> SVG
dynamicArrow col t = mkGroup [
    mkClipPath "maskLabel" $ removeGroups mask,
    withClipPathRef (Ref "maskLabel") $ withFillColorPixel col $ withStrokeColorPixel col $ withFillOpacity 0 $ withStrokeWidth 0.07 $ translate 0 yFinal $ fullArrow arrowLength
    ]
    where
    arrowLength = 1.5
    mask = translate 0 ((total_arrowLength/2) + maskYOffset) $ mkRect 2 (total_arrowLength+0.09) --animated mask
    total_arrowLength = arrowLength + 0.2
    yFinal 
        | t <= 0 = t * total_arrowLength * 1.07
        | otherwise = 0
    maskYOffset
        | t <= 0 = 0
        | otherwise = t * total_arrowLength * 1.07
        
counter :: Integer -> SVG
counter i = withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack (show i)
