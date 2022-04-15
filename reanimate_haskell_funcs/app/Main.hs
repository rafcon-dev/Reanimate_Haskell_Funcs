{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Reanimate
import qualified Rafael_length as Rafael_length
import qualified Rafael_reverse as Rafael_reverse
import qualified Rafael_reverse_hangs as Rafael_reverse_hangs
import qualified Rafael_sum as Rafael_sum
import qualified Rafael_product as Rafael_product
import qualified Rafael_maximum as Rafael_maximum
import qualified Rafael_minimum as Rafael_minimum

main :: IO ()
main = reanimate Rafael_sum.animation
