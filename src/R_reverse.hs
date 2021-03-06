{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module R_reverse (animation) where
import Codec.Picture --(PixelRGBA8)
import Control.Lens
import Control.Monad (forM, forM_, when)
import Linear.Vector
import Linear.V2
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene
import Reanimate.Svg
import Reanimate.ColorComponents
import Graphics.SvgTree


import qualified Data.Text as L
import qualified Data.List as DL
import Data.Text.Encoding
import Data.Maybe as M
import Data.Char(isSpace)

import Common.Others
import Common.Utils
import Common.AnimUtils
import Common.Assets




--SVG OBJECTS DEFINITION-------------------------------------------------------------------------------------------------

funcNameStr :: String
funcNameStr = "reverse"

inputLabelStr :: String
inputLabelStr = "input"

inputLength :: Int
inputLength = 4

inputStr :: String
inputStr = showCharArray $ reverse $ take inputLength ['z','y'..]

outputStr :: String
outputStr = showCharArray $ take inputLength ['z','y'..]

par1NameStr :: String
par1NameStr = "[a]"

par2NameStr :: String
par2NameStr = "[a]"

inputHeight = 3.0
inputScale = 0.7

sigHeight = 4.0
sigScale = 0.7

-- InputFull
inputFull =     
        withColFromToPixel tokensColorPixel equal equal $         
        withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
        where
            equal = length inputLabelStr
            txt = inputLabelStr ++ " = " ++ inputStr

funcEnd = length funcNameStr - 1

-- Function signature
sig = get1ParFunctionSignature funcNameStr par1NameStr par2NameStr

--Full Function 

txtStartToEqual = funcNameStr ++ " " ++ inputLabelStr ++ " ="
txtEqualToEnd = " " ++ funcNameStr ++ " " ++ inputStr
txtEqualToEndOutput = " " ++ outputStr

funcFull = get1ParExpandedFunction funcNameStr inputLabelStr inputStr

funcFullAtEnd = getFunctionAtEnd funcNameStr inputLabelStr outputStr
    
equalGlyphPos = -1 + length $ filter (/= ' ') txtStartToEqual

funcLeftOfEqual = snd $ splitGlyphs [0,1..equalGlyphPos] funcFull
funcRightOfEqual = snd $ splitGlyphs [s,s+1..s+e] funcFull
    where
        s = equalGlyphPos + 1
        e = s + ( length $ filter (/= ' ') txtEqualToEnd)
        
funcNameRight = snd $ splitGlyphs [s,s+1..s + length funcNameStr - 1] funcFull
    where
        s = equalGlyphPos + 1
        
brackets = snd $ splitGlyphs [s,s+2..s + length inputStr] funcFull
    where
        s = equalGlyphPos + length funcNameStr + 1
        
letters = reverse $ map (\i -> snd $ splitGlyphs [i] lettersTogether) [0,1..inputLength-1]
    where
        lettersTogether = snd $ splitGlyphs [s,s+2..s + length inputStr-1] funcFull
        s = equalGlyphPos + length funcNameStr + 2
--
        
----------------------------------------------------------------------------------------------------------------------

--ANIMATION PER SE-----------------------------------------------------
env :: Animation -> Animation
env = addStatic bg

animation :: Animation
animation = env $ scene $ do
    
    --fadeIn-------------------------------------------------------------------------------------------------------
    obj_fadeOverlay <- oNew fadeOverlay
    adjustZ (\z -> 10) $ oShow obj_fadeOverlay
    oHideWith obj_fadeOverlay $ adjustDuration (*2). oFadeOut
    ---
    
    --show signature--------------------------------------------------------------------------------------------------
    obj_FullSignature <- oNew sig
    oShowWith obj_FullSignature oFadeIn
    --
    
    wait 0.3
     
    --show input line-------------------------------------------------------------------------------------
    obj_InputFull <- oNew inputFull; 
    oModifyS obj_InputFull $ oBottomY .= - 2.5
    oShowWith obj_InputFull oFadeIn
    --
    
    wait 0.8
    
    --move signature and input to the top------------------------------------------------------------------
    do waitOn $ forkAllWithLag 0.2
        [
        oMoveToAndScale obj_FullSignature (0, sigHeight) sigScale 1,
        oMoveToAndScale obj_InputFull (0, inputHeight) inputScale 1
        ]
    --
    
    wait 0.5
    
    --create copy of signature at the same position of the original one----------------------------------------------------------
    obj_FullSignatureCopy <- oNew sig
    fullBottomY <- oRead obj_FullSignature oBottomY
    fullScale <- oRead obj_FullSignature oScale
    oModifyS obj_FullSignatureCopy $ oScale .= fullScale >> oBottomY .= fullBottomY
    oShow obj_FullSignatureCopy
    --
    
    --move the copy of the signature to the center of the screen, while applying a bouncing effect on the input------------------
    do waitOn $ forkAllWithLag 0.15 [
        oMoveToAndScale obj_FullSignatureCopy (0, 0) 1 1,
        bounce obj_InputFull
        ]
    --
    
    -----------------------------------------------------------------------------------------------------------------
    ----We now have the signature at the top, the input right below, and a split copy of the signature in the middle of the screen
    ------------------------------------------------------------------------------------------------------------------
    
    --Make arrow appear to illustrate transfering of input to function signature--------------------------------------------------
    arrowVar <- newVar 0
    sprite_Arrow <- newSprite $ dynamicArrow textColorPixel <$> unVar arrowVar
    spriteMap sprite_Arrow $ rotate 180
    spriteMap sprite_Arrow $ translate 0 (inputHeight-0.5)

    waitOn $ tweenVar arrowVar 1.2 $ \val -> (-1) + curveS 2
    --
    
    wait 0.1

    
    --Transform the function signature to the expanded function------------------------------------------------------------------------
    
    --First create two separate parts of the expanded function, so we can edit the right part of the equal sign. Don't show them still.
    obj_FuncFull <- oNew funcFull --to extract correct coordinates from


    --Animate the transformation
    waitOn $ forkAllWithLag 0.0
        [
        oHideWith obj_FullSignatureCopy $ adjustDuration (*2) . oScaleOut
        ,tweenVar arrowVar 1 $ \val -> curveS 2
        ,oShowWith obj_FuncFull $ adjustDuration (*1.5) . oScaleIn
        ]
            --
    
    -------------------------------------------------------------------------------------------------------------
    --We now have an expanded function in the middle of the screen (i.e.: reverse input = reverse [x,y,z])
    ----------------------------------------------------------------------------------------------------------------
    
    --replace full function with separate parts seamlessly
    obj_funcLeftOfEqual <- oNew funcLeftOfEqual
    obj_funcNameRight  <- oNew funcNameRight
    obj_brackets  <- oNew brackets
    
    obj_letters <- mapM oNew letters
    mapM oShow obj_letters
    
    oHide obj_FuncFull
    
    oShow obj_funcLeftOfEqual
    oShow obj_funcNameRight
    oShow obj_brackets
    mapM oShow obj_letters
    
    --cleanup
    destroySprite sprite_Arrow

    
    --Function working animation----------------------------------------------------------------------------------------
    
            
    wait 0.1
    
    waitOn $ forkAllWithDifferentLags [0.2, 0.1,0.0,0.1]
        [
        bounceOnX obj_funcNameRight
        ,bounceOnX obj_brackets
        ,forkAll $ map bounceOnX obj_letters
        ,oMoveFlipObjects_OnAnArch (zip obj_letters (reverse obj_letters)) 0.7 0.3 2.5
        ]
    
    wait 0.2
    
    obj_funcFullAtEnd <- oNew funcFullAtEnd --to get coordinates from
    funcFullAtEndLeft <- oRead obj_funcFullAtEnd oLeftX 
    funcFullAtEndRight <- oRead obj_funcFullAtEnd oRightX 
        
    waitOn $ forkAllWithDifferentLags [1.5,0,0]
        [
        oHideWith obj_funcNameRight $ adjustDuration (*1.5). oFadeOut
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo obj_brackets obj_letters obj_funcFullAtEnd 1
        ]
        
    
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    

