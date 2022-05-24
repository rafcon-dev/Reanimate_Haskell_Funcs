{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module R_length (animation) where
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
funcNameStr = "length"

inputLabelStr :: String
inputLabelStr = "input"

inputLength :: Int
inputLength = 3

inputStr :: String
inputStr = "[" ++ DL.intersperse ',' arr ++ "]"
    where
        arr = reverse $ take inputLength ['z','y'..]

par1NameStr :: String
par1NameStr = "[a]"

par2NameStr :: String
par2NameStr = "[a]"


-- InputFull
inputFull =     
        withColFromToPixel tokensColorPixel equal equal $         
        withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
        where
            equal = length inputLabelStr
            txt = inputLabelStr ++ " = " ++ inputStr
inputHeight = 3.0
inputScale = 0.7


funcEnd = length funcNameStr - 1
        
        
-- Function signature
sig = get1ParFunctionSignature funcNameStr par1NameStr par2NameStr
        
sigHeight = 4.0
sigScale = 0.7
    
--Full Function 

txtStartToEqual = funcNameStr ++ " " ++ inputLabelStr ++ " ="
txtEqualToEnd = " " ++ funcNameStr ++ " " ++ inputStr

funcFull = get1ParExpandedFunction funcNameStr inputLabelStr inputStr

funcFullAtEnd = getFunctionAtEnd funcNameStr inputLabelStr $ show inputLength
    
equalGlyphPos2 = -1 + length $ filter (/= ' ') txtStartToEqual

funcLeftOfEqual = snd $ splitGlyphs [0,1..equalGlyphPos2] funcFull
funcRightOfEqual = snd $ splitGlyphs [s,s+1..s+e] funcFull
    where
        s = equalGlyphPos2 + 1
        e = s + ( length $ filter (/= ' ') txtEqualToEnd)

funcNameRight = snd $ splitGlyphs [s,s+1..s + length funcNameStr - 1] funcFull
    where
        s = equalGlyphPos2 + 1
        
brackets = snd $ splitGlyphs [s,s+2..s + length inputStr] funcFull
    where
        s = equalGlyphPos2 + length funcNameStr + 1
        
letters = reverse $ map (\i -> snd $ splitGlyphs [i] lettersTogether) [0,1..inputLength-1]
    where
        lettersTogether = snd $ splitGlyphs [s,s+2..s + length inputStr-1] funcFull
        s = equalGlyphPos2 + length funcNameStr + 2
        
--
-------------------------------------------------------------------------------------------------------------------------------

--ANIMATION PER SE-----------------------------------------------------
env :: Animation -> Animation
env = addStatic bg

animation :: Animation
animation = env $ scene $ do
    
    cam <- newObject Camera
        
    --fadeIn--------------------------------------------------------------------------------------------------------------------
    obj_fadeOverlay <- oNew fadeOverlay
    adjustZ (\z -> 10) $ oShow obj_fadeOverlay
    oHideWith obj_fadeOverlay $ adjustDuration (*2). oFadeOut
    ---

    --show signature-------------------------------------------------------------------------------------------------------------
    obj_FullSignature <- oNewWithCam sig cam
    oShowWith obj_FullSignature oFadeIn
    --
    
    wait 0.3
    
    --show input line------------------------------------------------------------------------------------------------------------
    obj_InputFull <- oNewWithCam inputFull cam
    oModifyS obj_InputFull $ oBottomY .= - 2.5
    oShowWith obj_InputFull oFadeIn
    --
    
    wait 0.8
    
    --move signature and input to the top----------------------------------------------------------------------------------------
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
    
    --move the copy of the signature to the center of the screen, while applying a bouncing effect on the input-------------------
    do waitOn $ forkAllWithLag 0.15 [
        oMoveToAndScale obj_FullSignatureCopy (0, 0) 1 1,
        bounce obj_InputFull
        ]
    --
    
    -------------------------------------------------------------------------------------------------------------------------------
    ----We now have the signature at the top, the input right below, and a split copy of the signature in the middle of the screen
    -------------------------------------------------------------------------------------------------------------------------------
    
    --Make arrow appear to illustrate transfering of input to function signature---------------------------------------------------
    arrowVar <- newVar 0
    sprite_Arrow <- newSprite $ dynamicArrow textColorPixel <$> unVar arrowVar
    spriteMap sprite_Arrow $ rotate 180
    spriteMap sprite_Arrow $ translate 0 (inputHeight-0.5)

    waitOn $ tweenVar arrowVar 1.2 $ \val -> (-1) + curveS 2
    --
    
    wait 0.2

    
    --Transform the function signature to the expanded function------------------------------------------------------------------------
    
    --First create two separate parts of the expanded function, so we can edit the right part of the equal sign. Don't show them still.
    obj_FuncFull <- oNew funcFull --to extract correct coordinates from
    
    obj_funcLeftOfEqual <- oNew funcLeftOfEqual
    --funcLeftX <- oRead objFuncFull oLeftX
    --oModifyS obj_funcLeftOfEqual $ oLeftX .= funcLeftX
    
    obj_funcRightOfEqual <- oNew funcRightOfEqual
    --funcRightX <- oRead objFuncFull oRightX
    --oModifyS obj_funcRightOfEqual $ oRightX .= funcRightX
    

    --Animate the transformation
    waitOn $ forkAllWithLag 0.0
        [
        oHideWith obj_FullSignatureCopy $ adjustDuration (*2) . oScaleOut
        ,tweenVar arrowVar 1 $ \val -> curveS 2
        ,oShowWith obj_FuncFull $ adjustDuration (*1.5) . oScaleIn
        ]
            --
    
    ------------------------------------------------------------------------------------------------------------------------------
    --We now have an expanded function in the middle of the screen (i.e.: lenght input = lenght [x,y,z])
    ------------------------------------------------------------------------------------------------------------------------------
    
    --replace full function with separate parts seamlessly
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

    
    --Function working animation-------------------------------------------------------------------------------------------------
    
    --create counter object from inside the function name, and move it below the input array
    obj_counter <- oNew $ counter 0
    
    functionNameRightCenterX <- oRead obj_funcNameRight oCenterX
    oModifyS obj_counter $ oTranslateX .= functionNameRightCenterX >> oTranslateY .= 0 >> oScale .= 0
    
    oShow obj_counter
    
    --the counter object will iterate over all the input array letters, incrementing it's value on each time
    let obj_letters_withINdex = zip obj_letters [0 ..] --zip with the corresponding index, so we can update the counter value
    
    --repeatedly increment the counter object for each letter
    waitOn $ forkAllWithDifferentLags [0.1,0.2,0.3]
        [bounceOnY obj_funcNameRight,
        oMoveToAndScale obj_counter (functionNameRightCenterX, -1) 1 0.8
        ,countTheElementsAnimation obj_counter obj_letters_withINdex (-1)
        ]
    
    --create a final counter object, as the last function ended without showing it
    final_obj_counter <- oNew $ counter ((snd $ last $ obj_letters_withINdex)+1)
    lastLetterX <- oRead (fst $ last $ obj_letters_withINdex) oCenterX
    oModify final_obj_counter $ oTranslate .~ (V2 lastLetterX (-1))
    oShow final_obj_counter
    --
    
    --replace the right side of the equation back with the original full version with letters, so we can smoothly animate it all
    --oHide obj_funcRightOfEqualWithoutLetters
    
    mapM oHide obj_letters
    oHide obj_brackets
    oHide obj_funcNameRight
    oShow obj_funcRightOfEqual
    --
    
    obj_funcFullAtEnd <- oNew funcFullAtEnd --to get coordinates from
        
    --hide the right part of the equation, while moving the counter object to the correct position after the equal sign, and the left side of the equation
    waitOn $ forkAllWithDifferentLags [0.6, 0.2,0,0.6]
        [
        oHideWith obj_funcRightOfEqual $ adjustDuration (*2) . oScaleOut
        ,oMoveToNewY final_obj_counter 0 1
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo final_obj_counter [] obj_funcFullAtEnd 1
        ]
    --
    
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    
--the repeating animation for the counter. Basically a for cycle. obj_counter should start at the correct y value already
countTheElementsAnimation :: Object s a -> [(Object s Tree, Integer)] -> Double -> Scene s ()
countTheElementsAnimation obj_counter [] _= do wait 0.0
countTheElementsAnimation obj_counter (x:xs) counterYPos = do
                                                counterXPos <- oRead (fst $ x) oCenterX
                                                
                                                --translate counter object to the same x as the relevant element of the array
                                                oShow obj_counter
                                                waitOn $ oMoveToNewX obj_counter counterXPos 0.8
                                                --animate an "interaction" between the counter and the array element
                                                forkAll
                                                    [
                                                    bounceOnY $ fst $ x
                                                    ,bounceScaleUp obj_counter
                                                    ]
                                                --
                                                wait 0.6
                                                
                                                --Replace the counter with a sprite, so we can make the rolling numbers animation
                                                oHide obj_counter
                                                v <- newVar 0
                                                s <- newSprite $ numberRoll (snd $ x) <$> unVar v
                                                spriteMap s (translate counterXPos counterYPos) --put sprite at correct position
                                                waitOn $ tweenVar v 0.8 $ \val -> curveS 2 --animate the number roll
                                                wait 0.1
                                                destroySprite s
                                                
                                                --create a new object to pass again to the next iteration, with the correct value
                                                obj_NewCounter <- oNew $ counter ((snd $ x)+1)
                                                oModify obj_NewCounter $ oTranslate .~ (V2 counterXPos counterYPos)
                                                countTheElementsAnimation obj_NewCounter xs counterYPos--iterate again, until the array ends
                                                return ()
                                                          
