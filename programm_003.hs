#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

import Data.List
import System.Random


type Point = (Int, Int)

randomPoints :: Int -> Int -> StdGen -> [Point]
randomPoints 0 _ _ = []
randomPoints size max gen =
  let (first, newGen)   = randomR (0, max) gen
      (second, nextGen) = randomR (0, max) newGen
  in (first, second) : randomPoints (size - 1) max nextGen
