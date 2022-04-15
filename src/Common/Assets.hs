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
