{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Reanimate
import Reanimate.Render
import qualified R_length as R_length
import qualified R_reverse as R_reverse
import qualified R_reverse_hangs as R_reverse_hangs
import qualified R_sum as R_sum
import qualified R_product as R_product
import qualified R_maximum as R_maximum
import qualified R_minimum as R_minimum

main :: IO ()
--main = reanimate Rafael_sum.animation

main = sequence_ $ renderAsGifStd animsPaths
    where
        animsPaths =
            [
            (R_sum.animation, "sum.gif")
            ,(R_length.animation, "length.gif")
            ,(R_product.animation, "product.gif")
            ,(R_minimum.animation, "minimum.gif")
            ,(R_maximum.animation, "maximum.gif")
            ,(R_reverse.animation, "reverse.gif")
            ,(R_reverse_hangs.animation, "reverse_hangs.gif")
            ]

renderAsGifStd :: [(Animation, FilePath)] -> [IO ()]
renderAsGifStd animsPaths = map (\ap -> render (fst ap) (snd ap) RasterNone RenderGif 320 180 24 True) animsPaths
