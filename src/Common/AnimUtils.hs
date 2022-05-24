{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.AnimUtils
    (
    bounce
    ,bounceOnY
    ,bounceOnYDown
    ,bounceOnX
    ,bounceScaleUp
    ,dampenedSineWave
    ,oMoveToAndScale
    ,oMoveToXAndScale
    ,oMoveToYAndScale
    ,oMoveTo
    ,oMoveByVector
    ,oMoveToNewX
    ,oMoveToNewY
    ,oTransformWithScalingY
    ,oAlignLeftTo
    ,oAlignRightTo
    ,oMoveByX_OnAnArch
    ,oMoveToNewX_OnAnArch
    ,oMoveFlipObjects_OnAnArch
    ,getXCenters
    ,oScaleTo
    ,oShowWithFadeInSliding
    ,fadeOutObjs
    ,alignLeftSVGTo
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

import Common.Others
import Common.Utils
--EFFECTS------------------------------------------------------------------------------------------------------------

--
bounce :: Object s a -> Scene s ()
bounce obj = do fork $ oTweenContext obj 1.2 $ \t -> scaleXY (1) (1 + dampenedSineWave t)

--makes object bounce up and down a bitob
bounceOnY :: Object s a -> Scene s ()
bounceOnY obj = do fork $ oTweenContext obj 1.2 $ \t -> translate (0) (-0.3 * dampenedSineWave t) 
                   
bounceOnYDown :: Object s a -> Scene s ()
bounceOnYDown obj = do fork $ oTweenContext obj 1.2 $ \t -> translate (0) (0.3 * dampenedSineWave t) 

--makes object bounce left and right a bit
bounceOnX :: Object s a -> Scene s ()
bounceOnX obj = do fork $ oTweenContext obj 1.2 $ \t -> translate (-0.3 * dampenedSineWave t) (0) 

--makes object squash and stretch up and down for a bit, like a cartoon
bounceScaleUp :: Object s a -> Scene s ()
bounceScaleUp obj = do fork $ oTweenContext obj 1.2 $ \t -> scaleXY (newScale t) (newScale t)
                       where newScale t = 1 + (-0.7) * dampenedSineWave t

--returns a sine wave that gradually shrinks it's amplitude, i.e., just oscillates a few times and gradually stops                    
dampenedSineWave :: Double -> Double
dampenedSineWave t = -2.5**(-t/0.2) * (sin(t*12))

--------------------------------------------------------------------------------------------------------------------

--MOVEMENT WRAPPERS-----------------------------------------------------------------------------------------------

--animates the moving of an object to a position, while applying a scale factor to it 
oMoveToAndScale :: Object s a -> (Double, Double) -> Double -> Double -> Scene s ()
oMoveToAndScale obj (x, y) scaleFactor dur = do
                fork $ oTweenS obj dur $ \t -> do
                    oScale %= \prev -> fromToS prev scaleFactor t
                    oTranslateX %= \prev -> fromToS prev x t
                    oTranslateY %= \prev -> fromToS prev y t
       
oMoveToXAndScale :: Object s a ->  Double -> Double -> Double -> Scene s ()
oMoveToXAndScale obj x scaleFactor dur = do
                fork $ oTweenS obj dur $ \t -> do
                    oScale %= \prev -> fromToS prev scaleFactor t
                    oTranslateX %= \prev -> fromToS prev x t       
       
oMoveToYAndScale :: Object s a ->  Double -> Double -> Double -> Scene s ()
oMoveToYAndScale obj y scaleFactor dur = do
                fork $ oTweenS obj dur $ \t -> do
                    oScale %= \prev -> fromToS prev scaleFactor t
                    oTranslateY %= \prev -> fromToS prev y t
        
---animates the moving of an object to a position
oMoveTo :: Object s a -> (Double, Double) -> Double -> Scene s ()
oMoveTo obj (x, y) dur = do
        fork $ oTweenS obj dur $ \t -> do
            oTranslateX %= \prev -> fromToS prev x t
            oTranslateY %= \prev -> fromToS prev y t
    
---animates the moving of an object by a specified vector translation amount (i.e., an object at (2,4), moved by the vector (1,3), will end up at (4,7)
oMoveByVector :: Object s a -> (Double, Double) -> Double -> Scene s ()
oMoveByVector obj (x, y) dur = do
    originalX <- oRead obj oTranslateX
    originalY <- oRead obj oTranslateY
    oMoveTo  obj (originalX + x, originalY + y) dur
    
oMoveByX:: Object s a -> Double -> Double -> Scene s ()
oMoveByX obj x dur = do
    originalX <- oRead obj oTranslateX
    oMoveToNewX  obj (originalX + x) dur
    
---animates the moving of an object to a certain x coordinate, while keeping y intact
oMoveToNewX :: Object s a -> Double -> Double -> Scene s () --moves object to x position
oMoveToNewX obj x dur = do
    oTweenS obj dur $ \t -> do
            oTranslateX %= \prev -> fromToS prev x t
    
---animates the moving of an object to a certain y coordinate, while keeping x intact
oMoveToNewY :: Object s a -> Double -> Double -> Scene s () --moves object to y position
oMoveToNewY obj y dur = do
    oTweenS obj dur $ \t -> do
            oTranslateY %= \prev -> fromToS prev y t
    
oScaleTo :: Object s a -> Double -> Double -> Scene s ()
oScaleTo obj scaleFactor dur= do
                fork $ oTweenS obj dur $ \t -> do
                    oScale %= \prev -> fromToS prev scaleFactor t
                    
oScaleToX :: Object s a -> Double -> Double -> Scene s ()
oScaleToX obj xScale dur= do
                            (fork $ oTweenContext obj dur $ \t -> scaleXY (xScale) (1-t))
                    
oScaleToY :: Object s a -> Double -> Double -> Scene s ()
oScaleToY obj scaleFactor dur= do
                fork $ oTweenS obj dur $ \t -> do
                    oScale %= \prev -> fromToS prev scaleFactor t                            
                            
--transform one object into another with a vertical scaling effect (one shrinks in the y while the other grows)
oTransformWithScalingY :: Object s a -> Object s a -> Scene s ()
oTransformWithScalingY obj1 obj2 = do
                                    oShow obj2
                                    forkAllWithLag 0.0 
                                        [(fork $ oTweenContext obj1 1 $ \t -> scaleXY (1) (1-t))                                      
                                        ,(fork $ oTweenContext obj2 1 $ \t -> scaleXY (1) (t))]
                                    
--moves object so the left of its bounding box coincides with the left of the target's bounding box
oAlignLeftTo :: Object s a -> Object s a -> Double -> Scene s()
oAlignLeftTo obj target dur = do
                                objLeft <- oRead obj oLeftX 
                                targetLeft <- oRead target oLeftX 
                                oMoveByX obj (targetLeft - objLeft) dur

--moves object so the right of its bounding box coincides with the right of the target's bounding box
oAlignRightTo :: Object s a -> [Object s a] -> Object s a -> Double -> Scene s()
oAlignRightTo objParent objsChilds target dur = do
                                objRight <- oRead objParent oRightX
                                targetRight <- oRead target oRightX
                                forkAll $ map (\obj -> oMoveByX obj (targetRight - objRight) dur) $ [objParent] ++ objsChilds
                                
alignLeftSVGTo :: Tree -> Tree -> Tree
alignLeftSVGTo svg target = translate (minXTarget-minXOriginal) 0 svg
    where
        (minXTarget, _, _, _) = boundingBox target
        (minXOriginal, _, _, _) = boundingBox svg
        
--rotate around center, while keeping object orientation (translate in an arch)        
            
oMoveByX_OnAnArch :: Object s a ->  Double -> Double -> Double -> Scene s ()
oMoveByX_OnAnArch obj x archHeight dur= do 
    originalX <- oRead obj oCenterX
    originalY <- oRead obj oCenterY
    fork $ oTweenContext obj dur $ \t -> translate ( newX $ t ) ( newY $ t)
        where
            cart t = polarCoordToCartesian (x/2) ((t*180 + 180) * (-1))
            newX t = (vX $ cart t) + x/2
            newY t = (vY $ cart t) * archHeight
            
oMoveToNewX_OnAnArch :: Object s a ->  Double -> Double -> Double -> Scene s ()
oMoveToNewX_OnAnArch obj x archHeight dur = do 
    originalX <- oRead obj oCenterX
    oMoveByX_OnAnArch obj (x - originalX) archHeight dur
                                                            
oMoveFlipObjects_OnAnArch :: [(Object s Tree, Object s Tree)] -> Double -> Double -> Double -> Scene s ()
oMoveFlipObjects_OnAnArch  [] _ _ _ = do wait 0.0
oMoveFlipObjects_OnAnArch  (x:xs) archHeight forkLag dur = do
                                                    targetX <- oRead (snd $ x) oCenterX
                                                    forkAllWithLag forkLag $ [oMoveToNewX_OnAnArch (fst $ x) targetX archHeight dur, oMoveFlipObjects_OnAnArch xs archHeight forkLag dur]
                                                    return ()
                                                    
getXCenters :: [Object s a] -> Scene s [Double]
getXCenters = traverse (\obj -> oRead obj oCenterX)

--shows object with a smooth sliding down and fading effect
oShowWithFadeInSliding :: Object s a -> Double -> Scene s()
oShowWithFadeInSliding obj dur = do
                                    waitOn $ oMoveByVector obj (0, 0.3) 0.1
                                    forkAllWithLag 0.2
                                        [
                                        oMoveByVector obj (0, -0.3) dur
                                        ,oShowWith obj $ adjustDuration (*dur). oFadeIn
                                        ]
--fades out an array of objects
fadeOutObjs :: [Object s a] -> Double -> Scene s () 
fadeOutObjs objs dur = do
                    forkAll $ map (\obj -> oHideWith obj $ adjustDuration (*dur). oFadeOut) objs
---------------------------------------------------------------------------------------------------------------------

--REMINDER:oTranslateX is the x position of the origin of the object.
-----------oCenterX is the x position of the center of the bounding box of the svg drawing
-----------an object can have oTranslate=0, but have it's drawing in any place, and the center of that "place" is oCenter
-----------functions that animate the context (and use functions like translate, scale, etc), are effectively
-----------changing the SVG drawing, while functions that animate the oTranslate values for example, are changing
-----------the object that contains said SVG. A bit like Blender's Object and Edit mode.
