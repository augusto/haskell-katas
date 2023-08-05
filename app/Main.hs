module Main (main) where

import TenPinBowling

main :: IO ()
main = print $ score [Roll 5]
