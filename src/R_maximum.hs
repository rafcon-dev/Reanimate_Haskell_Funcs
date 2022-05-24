{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module R_maximum (animation) where
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
funcNameStr = "max"

inputLabelStr :: String
inputLabelStr = "input"

inputLength :: Int
inputLength = 4

inputArr :: [Integer]
inputArr = [-20,20,7,6]

inputStr :: String
inputStr = show inputArr

outputStr :: String
outputStr = show $ maximum inputArr

par1NameStr :: String
par1NameStr = "[a]"

par2NameStr :: String
par2NameStr = "a"

opStr :: String
opStr = "max?"

maxFoundStr :: String
maxFoundStr = "max!"

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


operator = scale 0.5 $ withColFromToPixel funcColorPixel 0 end $ center $ latexCfg calligraCfg $ L.pack opStr
    where
        end = (length opStr) - 1
        
maxFound = scale 0.5 $ withColFromToPixel funcColorPixel 0 end $ center $ latexCfg calligraCfg $ L.pack maxFoundStr
    where
        end = (length maxFoundStr) -1
        
lessThanQuestion = mkGroup
                    [
                    withColFromToPixel funcColorPixel 0 0 $ center $ latexCfg calligraCfg $ L.pack $ "<"
                    ,scale 0.7 $ translate 0 0.6 $ withColFromToPixel funcColorPixel 0 0 $ center $ latexCfg calligraCfg $ L.pack $ "?"
                    ]
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
    
    obj_values <- traverse oNewWithSvgPos values
    
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
    
    functionNameRightCenterX <- oRead obj_funcNameRight oCenterX
    
    obj_op <- oNew operator
    oModifyS obj_op $ oTranslateX .= functionNameRightCenterX >> oTranslateY .= 0 >> oScale .= 0
    oShow obj_op
    

    
    --show max operator
    waitOn $ forkAllWithDifferentLags [0.1,1.0]
        [
        bounceOnYDown obj_funcNameRight
        ,oMoveToAndScale obj_op (functionNameRightCenterX, -0.8) 1 1
        ]
    
    firstElX <- oRead (head obj_values) oTranslateX
    waitOn $ oMoveToNewX obj_op firstElX 0.8
    
    obj_maxFound <- oNew maxFound
    
    oModifyS obj_maxFound $ oTranslateX .= firstElX >> oTranslateY .= 0 >> oScale .= 0
    oShow obj_maxFound
    
    waitOn $ forkAllWithDifferentLags [0.1,0.0,0]
        [
        bounceScaleUp obj_op
        ,oMoveToNewY (head obj_values) 1 0.4
        ,oMoveToAndScale obj_maxFound (firstElX, 1.7) 1 0.5
        ]
     
    
    let obj_array_withOriginalArray = zip obj_values inputArr
        
    waitOn $ findMaximumAnimation (head obj_values) obj_op (tail obj_array_withOriginalArray) (head inputArr)
    
   -- waitOn $ forkAllWithDifferentLags [0.2, 0.1,0.0,0.1]
   --     [
   --     bounceOnX obj_funcNameRight
    --    ,bounceOnX obj_brackets
   --     ,forkAll $ map bounceOnX obj_letters
   --     ,oMoveFlipObjects_OnAnArch2 (zip obj_letters (reverse obj_letters)) 0.7 0.3 2.5
   --     ]
   
    obj_result <- oNew $ counter $ maximum inputArr
    let max_Index = indexOfMaximum inputArr
    resultX <- oRead (fst $ obj_array_withOriginalArray!!max_Index) oTranslateX
    resultY <- oRead (fst $ obj_array_withOriginalArray!!max_Index) oTranslateY
    oModify obj_result $ oTranslate .~ (V2 resultX resultY)
    oShow obj_result
    oHide (fst $ obj_array_withOriginalArray!!max_Index) 
    
    wait 0.2
    
    obj_funcFullAtEnd <- oNew funcFullAtEnd --to get coordinates from
    funcFullAtEndLeft <- oRead obj_funcFullAtEnd oLeftX 
    funcFullAtEndRight <- oRead obj_funcFullAtEnd oRightX 
        
    waitOn $ forkAllWithDifferentLags [0,0,1.5,0,0]
        [
        oHideWith obj_funcNameRight $ adjustDuration (*1.5). oFadeOut
        ,oHideWith obj_brackets $ adjustDuration (*1.5). oFadeOut
        ,oHideWith obj_op $ adjustDuration (*1.5). oFadeOut
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo obj_result [] obj_funcFullAtEnd 1
        ,oAlignRightTo obj_maxFound [] obj_funcFullAtEnd 1
        ]
    
    waitOn $ forkAll
        [
        oMoveToNewY obj_result 0 1
        ,oMoveToYAndScale obj_maxFound 0.7 0 1
        ]
    
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    
    

--the repeating animation for the counter. Basically a for cycle. obj_acc should start at the correct y value already
findMaximumAnimation :: Object s a -> Object s Tree -> [(Object s Tree, Integer)] -> Integer -> Scene s ()
findMaximumAnimation _ _ [] _ = do wait 0.0
findMaximumAnimation obj_curMax obj_op  (x:xs) curMax = do
                                                curElementXPos <- oRead (fst x) oCenterX
                                                curElementWidth <- oRead (fst x) oBBWidth
                                                let curCache = curElementXPos
                                                curMaxXPos <- oRead obj_curMax oTranslateX
                                                curMaxWidth <- oRead obj_curMax oBBWidth
                                                curMax_MaxX <- oRead obj_curMax oRightX
                                                curMaxYPos <- oRead obj_curMax oTranslateY
                                                
                                                obj_lessThanQuestion <- oNew lessThanQuestion
                                                oModifyS obj_lessThanQuestion $ oTranslateX .= curMax_MaxX - 0.17 >> oTranslateY .= curMaxYPos
                                                oShow obj_lessThanQuestion
                                                
                                                lessThanQuestion_MaxX <- oRead obj_lessThanQuestion oTranslateX
                                                
                                                oModifyS obj_lessThanQuestion $ oScale .= 0
                                                --opYPos <- oRead obj_op oCenterY
                                                   
                                                --translate operator object to the same x as the relevant element of the array

                                                waitOn $ forkAll
                                                    [
                                                    oMoveToNewX obj_op (curElementXPos) 0.7
                                                    ]
                                                
                                                bounceScaleUp obj_op
                                                
                                                waitOn $ oMoveToNewY (fst x) (curMaxYPos) 0.4
                                                waitOn $ oMoveToNewX (fst x) (lessThanQuestion_MaxX + curElementWidth/2 + 0.35) 0.5
                                                
                                                
                                                
                                                waitOn $ oScaleTo obj_lessThanQuestion 1 0.3
                                                wait 0.5
                                                
                                                if (snd x) > curMax
                                                   then do
                                                        waitOn $ forkAllWithDifferentLags [0,1,0]
                                                            [
                                                            oScaleTo obj_lessThanQuestion 0 0.4
                                                            ,oHideWith obj_curMax $ adjustDuration (*1). oFadeOut
                                                            ,oMoveToNewX (fst x) (curMaxXPos) 0.6
                                                            
                                                            ]
                                                        findMaximumAnimation (fst x) obj_op xs (snd x)
                                                   else do
                                                        waitOn $ forkAllWithLag 0
                                                            [
                                                            oScaleTo obj_lessThanQuestion 0 0.4
                                                            ,oHideWith (fst x) $ adjustDuration (*1). oFadeOut
                                                            ]
                                                        findMaximumAnimation obj_curMax obj_op xs curMax
                                                ---wait 1.0
                                                --waitOn $ oMoveToNewX (fst $ x) (curCache) 0.5
                                                --waitOn $ oMoveToNewY (fst $ x) (0) 0.2
                                                --Replace the accumulator with a sprite, so we can make the rolling numbers animation
                                                
                                                --oHide obj_acc
                                                --v <- newVar 0
                                                --s <- newSprite $ numberRoll curVal <$> unVar v --start numberRoll at sum so far
                                                --spriteMap s (translate curElementXPos accYPos) --put sprite at correct position
                                                
                                                --wait 0.1
                                                --destroySprite s
                                                
                                                --create a new object to pass again to the next iteration, with the correct value
                                                --obj_NewMax <- oNew $ counter (curVal+(snd x))
                                                --oModify obj_NewAcc $ oTranslate .~ (V2 curElementXPos accYPos)
                                                  --iterate again, until the array ends
                                                return ()
                                                
                                                
