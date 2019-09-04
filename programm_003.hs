#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

import Data.List
import System.Random


type Point = (Int, Int)

randomPoints :: Int -> [Point]
randomPoints size = let g      = mkStdGen 100
                        first  = take size (randoms g :: [Int])
                        second = take size (randoms g :: [Int])
                        in zip first second
