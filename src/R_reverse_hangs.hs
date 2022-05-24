{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module R_reverse_hangs (animation) where
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
import qualified Data.List.Split as DLS
import Data.Text.Encoding
import Data.Maybe as M
import Data.Char(isSpace, isDigit)

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
inputLength = 40

inputStr :: String
inputStr = "[1..]"

expandedInput :: String
expandedInput = show [1..inputLength]

outputStr :: String
outputStr = "hangs forever"

par1NameStr :: String
par1NameStr = "[a]"

par2NameStr :: String
par2NameStr = "[a]"

inputHeight = 3.0
inputScale = 0.7

sigHeight = 4.0
sigScale = 0.7

letterSpacing = 0.45
    
-- InputFull
inputFull =     
        withColFromToPixel tokensColorPixel equal equal $         
        withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
        where
            equal = length inputLabelStr
            txt = inputLabelStr ++ " = " ++ inputStr


-- Function signature
sig = get1ParFunctionSignature funcNameStr par1NameStr par2NameStr
            
--Full Function versions
txtStartToEqual = funcNameStr ++ " " ++ inputLabelStr ++ " ="
txtEqualToEnd = " " ++ funcNameStr ++ " " ++ inputStr
txtStartToEnd = txtStartToEqual ++ txtEqualToEnd
txtFinalStartToEnd = txtStartToEqual ++ " " ++ funcNameStr ++ " " ++ expandedInput
txtEqualToEndOutput = " " ++ outputStr

funcFull = get1ParExpandedFunction funcNameStr inputLabelStr inputStr

funcFullExpanded = get1ParExpandedFunction funcNameStr inputLabelStr expandedInput

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
        
output = snd $ splitGlyphs [s,s+1..s + length outputStr - 1] funcFullAtEnd
    where
        s = equalGlyphPos + 1
        
brackets = snd $ splitGlyphs bracketIndices funcFull
    where
        bracketIndices = DL.findIndices (\x -> x=='[') $ filter (/= ' ') $ txtStartToEnd
        s = equalGlyphPos + length funcNameStr + 1
        
letters = reverse $ map (\i -> snd $ splitGlyphs [i] lettersTogether) [0,1..inputLength-1]
    where
        lettersTogether = snd $ splitGlyphs [s,s+2..s + length inputStr-1] funcFull
        s = equalGlyphPos + length funcNameStr + 2

numbers = map (\i -> snd $ splitGlyphs i numbersTogether) indices
    where
        numbersTogether = snd $ splitGlyphs numbersIndices funcExpandedInput
        s = equalGlyphPos + length funcNameStr + 2
        numbersIndices = fmap (+(0)) $ DL.findIndices (\x -> isDigit x) $ filter (/= ' ') $ reverse $ expandedInput
        separated = DLS.splitOneOf ",[" $ filter (/= ' ') $ expandedInput
        lengths = (map (\s -> length s) $ tail separated)
        indices = map (\l -> take (fst l) [(snd l)..]) $ zip lengths iterator
        iterator = (scanl1 (+) ([0] ++ lengths))

separators =  map (\i -> snd $ splitGlyphs [i] numbersTogether) [0,1..inputLength-1]
    where
        numbersTogether = snd $ splitGlyphs numbersIndices funcExpandedInput
        numbersIndices = fmap (+(0)) $ DL.findIndices (\x -> x==',') $ filter (/= ' ') $ reverse $ expandedInput
        
ellipsis = snd $ splitGlyphs [totLength-3,totLength-2,totLength-1] funcFull
    where
        totLength = length $ filter (/= ' ') $ txtStartToEnd
        
funcExpandedInput = alignLeftSVGTo (snd $ splitGlyphs [s,s+1..s + length expandedInput-1] funcFullExpanded) brackets
    where
        s = M.fromMaybe 0 $ DL.findIndex (\x -> x=='[') $ filter (/= ' ') $ txtFinalStartToEnd
--   
----------------------------------------------------------------------------------------------------------------------

--ANIMATION PER SE-----------------------------------------------------
env :: Animation -> Animation
env = addStatic bg

animation :: Animation
animation = env $ scene $ do
    
    cam <- newObject Camera

    --fadeIn-------------------------------------------------------------------------------------------------------
    obj_fadeOverlay <- oNew fadeOverlay
    adjustZ (\z -> 10) $ oShow obj_fadeOverlay
    oHideWith obj_fadeOverlay $ adjustDuration (*2). oFadeOut
    ---
    
    --show signature--------------------------------------------------------------------------------------------------
    obj_FullSignature <- oNewWithCam sig cam
    oShowWith obj_FullSignature oFadeIn
    --
    
    wait 0.3

    --show input line-------------------------------------------------------------------------------------
    obj_InputFull <- oNewWithCam inputFull cam
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
    obj_FullSignatureCopy <- oNewWithCam sig cam
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
    
    wait 0.0

    --Transform the function signature to the expanded function------------------------------------------------------------------------
    
    --First create two separate parts of the expanded function, so we can edit the right part of the equal sign. Don't show them still.
    obj_FuncFull <- oNewWithCam funcFull cam --to extract correct coordinates from


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
    obj_funcLeftOfEqual <- oNewWithCam funcLeftOfEqual cam
    obj_funcNameRight  <- oNewWithCam funcNameRight cam
    obj_brackets  <- oNewWithCam brackets cam
    
    obj_numbers <- mapM (\n -> oNewWithSvgPosAndCam n cam) numbers
    obj_separators <- mapM (\s -> oNewWithSvgPosAndCam s cam) separators
    
    obj_ellipsis <- oNewWithSvgPosAndCam ellipsis cam
    oShow obj_ellipsis
    oHide obj_FuncFull
    
    oShow obj_funcLeftOfEqual
    oShow obj_funcNameRight
    oShow obj_brackets
    
    oShow $ obj_numbers!!0
    
    --cleanup
    destroySprite sprite_Arrow
    
    --Function working animation----------------------------------------------------------------------------------------
    
    wait 0.1
    
    --bounces the name of the function onto the array to mimic a "connection", plays the reverse animation, and follow with camera
    waitOn $ forkAllWithDifferentLags [0.2, 0.1,0,0,0,7]
        [
        bounceOnX obj_funcNameRight
        ,bounceOnX obj_brackets
        ,forkAll $ map bounceOnX obj_numbers
        ,reverseHangsAnimation (zip obj_numbers obj_separators) obj_ellipsis cam 0.14 0.19 2.2
        ,cameraZoom cam 6 0.75
        ,cameraPan cam 10 $ V2 25 0
        ,fadeOutObjs (obj_numbers) 1.5
        ]
    
    obj_result <- oNewWithSvgPosAndCam output cam
    obj_funcFullAtEnd <- oNewWithCam funcFullAtEnd cam --to get coordinates from
    funcFullAtEndLeft <- oRead obj_funcFullAtEnd oLeftX 
    funcFullAtEndRight <- oRead obj_funcFullAtEnd oRightX 
    
    oHide obj_funcNameRight
    
    --get camera back into the origin, fade out the commas, fadein the result
    waitOn $ forkAllWithDifferentLags [0,0,0,1,0,0]
        [
        cameraPan cam 2.5 $ V2 0 0
        ,fadeOutObjs (obj_separators ++ [obj_ellipsis, obj_brackets]) 1.5
        ,cameraZoom cam 3 1
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo obj_result [] obj_funcFullAtEnd 1
        ,oShowWithFadeInSliding obj_result 3
        ]
        
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    
--iteratively create the elements of the array one by one, speeding up on each one (hence the forkLag/1.15). The rest are magic numbers that work well together, by trial and error.
reverseHangsAnimation :: [(Object s Tree, Object s tree)] -> Object s Tree -> Object s Camera -> Double -> Double -> Double -> Scene s ()
reverseHangsAnimation  [] _ _ _ _ _ = do wait 0.0
reverseHangsAnimation  (x:xs) obj_ellipsis cam archHeight forkLag dur = do
                                                    curElementXPos <- oRead (fst x) oCenterX
                                                    
                                                    forkAllWithDifferentLags [forkLag*2.5,forkLag,forkLag,forkLag,forkLag] $
                                                        [
                                                        oMoveToNewX obj_ellipsis (curElementXPos+1.5) (forkLag*2.5) --slide right bracket
                                                        ,oShow $ fst x --show number
                                                        ,oShow $ snd x --show comma
                                                        ,oMoveToNewX_OnAnArch (fst x) (60+curElementXPos*(1/(3*forkLag))) archHeight dur --send number far right
                                                        ,reverseHangsAnimation xs obj_ellipsis cam archHeight (forkLag/1.15) dur --next number, same thing, but faster
                                                        ]
                                                    return ()                                                  
    
