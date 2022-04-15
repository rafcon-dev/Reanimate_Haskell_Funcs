{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Rafael_reverse_hangs (animation) where
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
funcNameStr = "r"

inputLabelStr :: String
inputLabelStr = "input"

inputLength :: Int
inputLength = 4

inputStr :: String
inputStr = "[1..]"

expandedInput :: String
expandedInput = "[1,23,3,4,..]"

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

--letterSpacing = snd $ splitGlyphs [0,1] $ latexCfg calligraCfg $ L.pack $ show $ "1,"
letterSpacing = 0.45
    
    
-- InputFull
inputFull =     
        withColFromToPixel tokensColorPixel equal equal $         
        withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
        where
            equal = length inputLabelStr
            txt = inputLabelStr ++ " = " ++ inputStr

funcEnd = length funcNameStr - 1

-- Function signature
sig = 
    withColFromToPixel funcColorPixel 0 funcEnd $
    withColFromToPixel funcColorPixel colonsStart colonsEnd $
    withColFromToPixel tokensColorPixel par1Start par1End $
    withColFromToPixel funcColorPixel arrow arrow $
    withColFromToPixel tokensColorPixel par2Start par2End $
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt
    where
        colonsStart = funcEnd + 1
        colonsEnd = colonsStart + 1
        par1Start = colonsEnd + 1
        par1End = par1Start -1 + length par1NameStr
        arrow = par1End + 1
        par2Start = arrow + 1
        par2End = par2Start -1 + length par2NameStr
        txt = funcNameStr ++ " :: " ++ par1NameStr ++ " $\\rightarrow$ " ++ par2NameStr
        

    
--Full Function 

txtStartToEqual = funcNameStr ++ " " ++ inputLabelStr ++ " ="
txtEqualToEnd = " " ++ funcNameStr ++ " " ++ inputStr
txtStartToEnd = txtStartToEqual ++ txtEqualToEnd
txtFinalStartToEnd = txtStartToEqual ++ " " ++ funcNameStr ++ " " ++ expandedInput
txtEqualToEndOutput = " " ++ outputStr

funcFull = 
    withColFromToPixel funcColorPixel 0 funcEnd $ 
    withColFromToPixel textColorPixel input1Start input1End $ 
    withColFromToPixel tokensColorPixel equalPos equalPos $ 
    withColFromToPixel funcColorPixel input2Start input2End $ 
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg txt
    where
        input1Start = funcEnd + 1
        input1End = input1Start - 1 + length inputLabelStr
        equalPos = funcEnd + length inputLabelStr + 1
        input2Start = equalPos + 1
        input2End = input2Start - 1 + length funcNameStr
        txt = L.pack $ txtStartToEqual ++ txtEqualToEnd
        
funcFullExpanded = 
    withColFromToPixel funcColorPixel 0 funcEnd $ 
    withColFromToPixel textColorPixel input1Start input1End $ 
    withColFromToPixel tokensColorPixel equalPos equalPos $ 
    withColFromToPixel funcColorPixel input2Start input2End $ 
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg txt
    where
        input1Start = funcEnd + 1
        input1End = input1Start - 1 + length inputLabelStr
        equalPos = funcEnd + length inputLabelStr + 1
        input2Start = equalPos + 1
        input2End = input2Start - 1 + length funcNameStr
        txt = L.pack $ txtFinalStartToEnd

funcFullAtEnd = 
    withColFromToPixel funcColorPixel 0 funcEnd $ 
    withColFromToPixel textColorPixel input1Start input1End $ 
    withColFromToPixel tokensColorPixel equalPos equalPos $ 
    withColFromToPixel funcColorPixel outputStart outputEnd $ 
    withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg txt
    where
        input1Start = funcEnd + 1
        input1End = input1Start - 1 + length inputLabelStr
        equalPos = funcEnd + length inputLabelStr + 1
        outputStart = equalPos + 1
        outputEnd = outputStart - 1 + length outputStr
        txt = L.pack $ txtStartToEqual ++ txtEqualToEndOutput

--funcExpandedInput = withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack expandedInput

equalGlyphPos2 = -1 + length $ filter (/= ' ') txtStartToEqual

funcLeftOfEqual = snd $ splitGlyphs [0,1..equalGlyphPos2] funcFull
funcRightOfEqual = snd $ splitGlyphs [s,s+1..s+e] funcFull
    where
        s = equalGlyphPos2 + 1
        e = s + ( length $ filter (/= ' ') txtEqualToEnd)
--funcRightOfEqual = fst $ splitGlyphs [0,1..equalGlyphPos2]  funcFull

funcNameRight = snd $ splitGlyphs [s,s+1..s + length funcNameStr - 1] funcFull
    where
        s = equalGlyphPos2 + 1
        
brackets = snd $ splitGlyphs bracketIndices funcFull
    where
        bracketIndices = DL.findIndices (\x -> x=='[') $ filter (/= ' ') $ txtStartToEnd
        s = equalGlyphPos2 + length funcNameStr + 1
        
letters = reverse $ map (\i -> snd $ splitGlyphs [i] lettersTogether) [0,1..inputLength-1]
    where
        lettersTogether = snd $ splitGlyphs [s,s+2..s + length inputStr-1] funcFull
        s = equalGlyphPos2 + length funcNameStr + 2
    
    
funcExpandedInput = alignLeftSVGTo (snd $ splitGlyphs [s,s+1..s + length expandedInput-1] funcFullExpanded) brackets
    where
        s = M.fromMaybe 0 $ DL.findIndex (\x -> x=='[') $ filter (/= ' ') $ txtFinalStartToEnd

numbers = map (\i -> snd $ splitGlyphs i numbersTogether) indices
    where
        numbersTogether = snd $ splitGlyphs numbersIndices funcExpandedInput
        s = equalGlyphPos2 + length funcNameStr + 2
        numbersIndices = fmap (+(0)) $ DL.findIndices (\x -> isDigit x) $ filter (/= ' ') $ reverse $ expandedInput
        
        separated = DLS.splitOn "," $ filter (/= ' ') $ reverse $ expandedInput
        lengths = [1] ++ [length $ separated!!1,length $ separated!!2..length $ last separated]
        --indices = map (\l -> take (fst l) [(snd l)..]) $ zip lengths iterator
        indices = [[0],[1,2],[3],[4]]
        iterator = [0] ++ (tail $ scanl (+) 1 lengths)
        
numbers2 = map (\i -> translate (xPos i) 0 $ svgNumber i) [1,2..inputLength]
    where
        xPos i= (fromIntegral i)*letterSpacing*2*(fromIntegral $ 1)
   
--separators = [svgFromText "["] ++ commas
  --  where
    --    commas = map (\i -> translate (xPos i) (-0.3) svgComma) [1,2..inputLength]
      --  xPos i = (fromIntegral i)*letterSpacing*2*(fromIntegral $ 1) + letterSpacing
      --  svgComma = svgFromText ","
        
--commas = map (\i -> translate (xPos i) 0 svgComma) [1,2..inputLength]
--    where
--        xPos i = (fromIntegral i)*letterSpacing*(fromIntegral $ nDigits i) + letterSpacing
--        svgComma = svgFromText ","

separators =  map (\i -> snd $ splitGlyphs [i] numbersTogether) [0,1..inputLength-1]
    where
        numbersTogether = snd $ splitGlyphs numbersIndices funcExpandedInput
        numbersIndices = fmap (+(0)) $ DL.findIndices (\x -> x==',') $ filter (/= ' ') $ reverse $ expandedInput
        
ellipsis = snd $ splitGlyphs [totLength-3,totLength-2,totLength-1] funcFull
    where
        totLength = length $ filter (/= ' ') $ txtStartToEnd
--

alignLeftSVGTo :: Tree -> Tree -> Tree
alignLeftSVGTo svg target = translate (minXTarget-minXOriginal) 0 svg
    where
        (minXTarget, _, _, _) = boundingBox target
        (minXOriginal, _, _, _) = boundingBox svg
        
nDigits n = toInteger (round (logBase 10 (fromIntegral n)) + 1)

svgFromText txt = withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack $ txt

svgNumber n = svgFromText $ show n

counter :: Integer -> SVG
counter i = withFillColorPixel textColorPixel $ center $ latexCfg calligraCfg $ L.pack (show i)
        
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
    
    obj_numbers <- mapM oNew numbers
    obj_separators <- mapM oNew separators
    
    obj_ellipsis <- oNew ellipsis
    oShow obj_ellipsis
    oHide obj_FuncFull
    
    oShow obj_funcLeftOfEqual
    oShow obj_funcNameRight
    oShow obj_brackets
    
    --obj_test <- oNew funcExpandedInput
    --oShow obj_test
    --oAlignLeftTo obj_funcLeftOfEqual obj_brackets 0
    
    --wait 2
    oShow $ obj_numbers!!0
    oShow $ obj_separators!!0
    --mapM oShow obj_separators
    --mapM oShow obj_letters
    
    --cleanup
    destroySprite sprite_Arrow

    
    --Function working animation----------------------------------------------------------------------------------------
    
            
    wait 0.1
    
    waitOn $ forkAllWithDifferentLags [0.2, 0.1,0.0,0.1]
        [
        bounceOnX obj_funcNameRight
        ,bounceOnX obj_brackets
        ,forkAll $ map bounceOnX obj_numbers
        ,reverseHangsAnimation (zip obj_numbers obj_separators) obj_ellipsis 0.2 0.3 2.5
        ]
    
    wait 0.2
    
    obj_funcFullAtEnd <- oNew funcFullAtEnd --to get coordinates from
    funcFullAtEndLeft <- oRead obj_funcFullAtEnd oLeftX 
    funcFullAtEndRight <- oRead obj_funcFullAtEnd oRightX 
        
    waitOn $ forkAllWithDifferentLags [1.5,0,0]
        [
        oHideWith obj_funcNameRight $ adjustDuration (*1.5). oFadeOut
        ,oAlignLeftTo obj_funcLeftOfEqual obj_funcFullAtEnd 1
        ,oAlignRightTo obj_brackets obj_numbers obj_funcFullAtEnd 1
        ]
        
    
    wait 1.3
    
    --smoothly fade out, as we're over
    obj_fadeOutOverlay <- oNew fadeOverlay
    
    oShowWith obj_fadeOutOverlay $ adjustDuration (*2). oFadeIn
    --
    
    
reverseHangsAnimation :: [(Object s Tree, Object s tree)] -> Object s Tree -> Double -> Double -> Double -> Scene s ()
reverseHangsAnimation  [] _ _ _ _ = do wait 0.0
reverseHangsAnimation  (x:xs) obj_ellipsis archHeight forkLag dur = do
                                                    --targetX <- oRead (snd $ x) oCenterX
                                                    curElementXPos <- oRead (fst x) oCenterX
                                                    oShow $ fst x
                                                    oShow $ snd x
                                                    --waitOn $ oMoveToNewX (fst x) targetX 0.8
                                                    forkAllWithLag forkLag $ [oMoveToNewX obj_ellipsis curElementXPos 0.8, oMoveToNewX_OnAnArch (fst x) 20 archHeight dur, reverseHangsAnimation xs obj_ellipsis archHeight forkLag dur]
                                                    return ()
                                                    
reverseHangsAnimation2 :: [(Object s Tree, Object s tree)] -> Object s Tree -> Double -> Double -> Double -> Scene s ()
reverseHangsAnimation2  [] _ _ _ _ = do wait 0.0
reverseHangsAnimation2  (x:xs) obj_ellipsis archHeight forkLag dur = do
                                                    --targetX <- oRead (snd $ x) oCenterX
                                                    curElementXPos <- oRead (fst x) oCenterX
                                                    oShow $ fst x
                                                    oShow $ snd x
                                                    --waitOn $ oMoveToNewX (fst x) targetX 0.8
                                                    forkAllWithLag forkLag $ [oMoveToNewX obj_ellipsis curElementXPos 0.8, oMoveToNewX_OnAnArch (fst x) 20 archHeight dur, reverseHangsAnimation2 xs obj_ellipsis archHeight forkLag dur]
                                                    return ()
    

