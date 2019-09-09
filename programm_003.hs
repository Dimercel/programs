#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

import Data.List
import System.Random


type Point = (Int, Int)


seed = 42 :: Int

randomPoints :: Int -> Int -> StdGen -> [Point]
randomPoints 0 _ _ = []
randomPoints size max gen =
  let (first, newGen)   = randomR (0, max) gen
      (second, nextGen) = randomR (0, max) newGen
  in (first, second) : randomPoints (size - 1) max nextGen

createLimits :: [Point] -> [Point]
createLimits [] = []
createLimits points =
  let maxX = maximum $ map fst points
      maxY = maximum $ map snd points
      minX = minimum $ map fst points
      minY = minimum $ map snd points
      hLimit from to y = [(x, y) | x <- [from .. to]]
      vLimit from to x = [(x, y) | y <- [from .. to]]
  in concat [
    hLimit (minX - 1) (maxX + 1) (minY - 1),
    hLimit (minX - 1) (maxX + 1) (maxY + 1),
    vLimit minY maxY (minX - 1),
    vLimit minY maxY (maxX + 1),
    points]

