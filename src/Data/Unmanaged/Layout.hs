{-# LANGUAGE ScopedTypeVariables #-}

module Data.Unmanaged.Layout
    ( align
    , composeAligment
    ) where

import Data.Bits

align :: Int -> Int -> Int
align a off = (off + a - 1) .&. (complement (a - 1))

composeAligment :: Int -> Int -> Int
composeAligment = max
