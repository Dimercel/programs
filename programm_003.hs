#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

import Data.List (filter)
import System.Random


type Point = (Int, Int)


seed = 42 :: Int

randomPoints :: Int -> Int -> StdGen -> [Point]
randomPoints 0 _ _ = []
randomPoints count max gen =
  let (first, newGen)   = randomR (0, max) gen
      (second, nextGen) = randomR (0, max) newGen
  in (first, second) : randomPoints (count - 1) max nextGen

createLimits :: Int -> Int -> [Point]
createLimits 0 _ = []
createLimits _ 0 = []
createLimits rowCount colCount =
  let hLimit from to row = [(row, col) | col <- [from .. to]]
      vLimit from to col = [(row, col) | row <- [from .. to]]
  in concat [
    hLimit (-1) colCount (-1),
    hLimit (-1) colCount rowCount,
    vLimit 0 (rowCount - 1) (-1),
    vLimit 0 (rowCount - 1) colCount
    ]

makeScene :: Int -> [Point]
makeScene size = createLimits size size ++ randomPoints size (size - 1) (mkStdGen seed)

sceneSize :: [Point] -> (Int, Int)
sceneSize points
  | length points <= 9 = (0, 0)
  | otherwise = (maximum (map fst points), maximum (map snd points))
