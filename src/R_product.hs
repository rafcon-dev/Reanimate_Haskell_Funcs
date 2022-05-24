{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module R_product (animation) where
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
import Data.Char(isSpace)

import Common.Others
import Common.Utils
import Common.AnimUtils
import Common.Assets

--SVG OBJECTS DEFINITION-------------------------------------------------------------------------------------------------

funcNameStr :: String
funcNameStr = "prod"

inputLabelStr :: String
inputLabelStr = "input"

inputLength :: Int
inputLength = 4

inputArr :: [Integer]
inputArr = [2,4,-3]

inputStr :: String
inputStr = show inputArr

outputStr :: String
outputStr = show $ product inputArr

par1NameStr :: String
par1NameStr = "[a]"

par2NameStr :: String
par2NameStr = "a"

opStr :: String
opStr = "*"

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
funcFullStr = txtStartToEqual ++ txtEqualToEnd

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
        
brackets = snd $ splitGlyphs indices funcFull
    where
        indices = DL.findIndices (\c -> c==',' || c=='[' || c== ']') $ filter (/= ' ') funcFullStr

values = map (\g -> snd $ splitGlyphs g funcFull) glyphGroups
    where
        s = equalGlyphPos + length funcNameStr + 1
        valLengths = map (length . show) inputArr
        valLenghts1 = map succ valLengths
        gStarts = map (\i -> i + s) $ scanl (+) 1 valLenghts1
        glyphGroups = map (\i -> [gStarts!!i..gStarts!!i + valLengths!!i - 1]) [0,1..(length inputArr)-1]

operator = withColFromToPixel funcColorPixel 0 0 $ center $ latexCfg calligraCfg $ L.pack opStr
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
    
    wait 0.1

    
    --Transform the function signature to the expanded function------------------------------------------------------------------------
    
    --First create two separate parts of the expanded function, so we can edit the right part of the equal sign. Don't show them still.
    obj_FuncFull <- oNewWithCam funcFull cam--to extract correct coordinates from


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
    
    obj_values <- mapM (\v -> oNewWithCam v cam) values
    mapM oShow obj_values
    
    oHide obj_FuncFull
    
    oShow obj_funcLeftOfEqual
    oShow obj_funcNameRight
    oShow obj_brackets
    mapM oShow obj_values
    
    --cleanup
    destroySprite sprite_Arrow

    
    --Function working animation----------------------------------------------------------------------------------------
    
            
    wait 0.1
    
    obj_acc <- oNewWithCam (counter 1) cam

    functionNameRightCenterX <- oRead obj_funcNameRight oCenterX
    oModifyS obj_acc $ oTranslateX .= functionNameRightCenterX >> oTranslateY .= 0 >> oScale .= 0
    
    obj_op <- oNewWithCam operator cam
    oModifyS obj_op $ oTranslateX .= functionNameRightCenterX >> oTranslateY .= 0 >> oScale .= 0
    oShow obj_acc
    oShow obj_op
    
    
    --show accumulator and operator
    waitOn $ forkAllWithDifferentLags [0.1, 0.1,1.0]
        [
        bounceOnY obj_funcNameRight,
        oMoveToAndScale obj_acc (functionNameRightCenterX, 1.6) 1 1
        ,oMoveToAndScale obj_op (functionNameRightCenterX, 0.8) 1 1
        ]
        
    let obj_array_withOriginalArray = zip obj_values inputArr
        
    waitOn $ multiplyTheElementsAnimation obj_acc obj_op obj_array_withOriginalArray 1
       
    obj_result <- oNewWithCam ( counter $ product inputArr ) cam
    lastValX <- oRead (fst $ last $ obj_array_withOriginalArray) oCenterX
    oModify obj_result $ oTranslate .~ (V2 lastValX 1.6)
    oShow obj_result
    
    wait 0.2
    
    obj_funcFullAtEnd <- oNewWithCam funcFullAtEnd cam--to get coordinates from
    funcFullAtEndLeft <- oRead obj_funcFullAtEnd oLeftX 
    funcFullAtEndRight <- oRead obj_funcFullAtEnd oRightX 
        
    waitOn $ forkAllWithDifferentLags [0,0,1.5,0,0.2]
        [
        oHideWith obj_funcNameRight $ adjustDuration (*1.5). oFadeOut
        ,oHideWith obj_brackets $ adjustDuration (*1.5). oFadeOut
        ,oHideWith obj_op $ adjustDuration (*1.5). oFadeOut
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo obj_result [] obj_funcFullAtEnd 1
        ,oMoveToNewY obj_result 0 1
        ]
        
    
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    
    
--ANIMATIONS--------------------------------------------------------------------------------------------------------
--REMINDER:oTranslateX is the x position of the origin of the object.
-----------oCenterX is the x position of the center of the bounding box of the svg drawing
-----------an object can have oTranslate=0, but have it's drawing in any place, and the center of that "place" is oCenter
-----------functions that animate the context (and use functions like translate, scale, etc), are effectively
-----------changing the SVG drawing, while functions that animate the oTranslate values for example, are changing
-----------the object that contains said SVG. A bit like Blender's Object and Edit mode.


--the repeating animation for the counter. Basically a for cycle. obj_acc should start at the correct y value already
multiplyTheElementsAnimation :: Object s a -> Object s Tree -> [(Object s Tree, Integer)] -> Integer -> Scene s ()
multiplyTheElementsAnimation _ _ [] _ = do wait 0.0
multiplyTheElementsAnimation obj_acc obj_op (x:xs) curVal = do
                                                curElementXPos <- oRead (fst $ x) oCenterX
                                                accYPos <- oRead obj_acc oCenterY
                                                opYPos <- oRead obj_op oCenterY
                                                   
                                                --translate accumulator object to the same x as the relevant element of the array
                                                oShow obj_acc
                                                waitOn $ forkAll
                                                    [
                                                    oMoveToNewX obj_acc curElementXPos 0.8
                                                    ,oMoveToNewX obj_op (curElementXPos) 0.8
                                                    ]
                                                
                                                --Replace the accumulator with a sprite, so we can make the rolling numbers animation
                                                
                                                oHide obj_acc
                                                v <- newVar 0
                                                s <- newSprite $ numberRoll curVal <$> unVar v --start numberRoll at product so far
                                                spriteMap s (translate curElementXPos accYPos) --put sprite at correct position
                                                let spriteOffsetToRoll = curVal*(snd x) - curVal
                                                waitOn $ forkAllWithLag 0.4
                                                    [
                                                    oMoveToAndScale (fst $ x) (curElementXPos, opYPos) 0 1 --attract the element to the operator sign and make it disappear
                                                    ,tweenVar v 1 $ \val -> fromToS 0.0 (fromIntegral $ spriteOffsetToRoll) . curveS 2 --animate the number roll
                                                    ]
                                                wait 0.1
                                                destroySprite s
                                                
                                                --create a new object to pass again to the next iteration, with the correct value
                                                obj_NewAcc <- oNew $ counter (curVal*(snd x))
                                                oModify obj_NewAcc $ oTranslate .~ (V2 curElementXPos accYPos)
                                                multiplyTheElementsAnimation obj_NewAcc obj_op xs (curVal*(snd x))  --iterate again, until the array ends
                                                return ()
                                                
                                                
