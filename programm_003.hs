#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

import Data.List
import Data.Set (fromList, member)
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
makeScene size
  | size > 1 = createLimits size size ++ randomPoints size (size - 1) (mkStdGen seed)
  | otherwise = makeScene 2

sceneSize :: [Point] -> Int
sceneSize points = maximum (map fst points)

freeSpaces :: [Point] -> [Point]
freeSpaces scene =
  let size = sceneSize scene
      spaces = [(row, col) | row <- [0 .. size - 1], col <- [0 .. size - 1]]
      pointsSet = fromList scene
  in foldr (\x acc -> if member x pointsSet then acc else x : acc) [] spaces

-- Теперь нам нужно определиться с начальной и конечной точками пути. Их мы также генерируем случайно,
-- на свободных местах в сцене.
endPoints :: [Point] -> (Point, Point)
endPoints scene =
  let spaces = freeSpaces scene
      [(startInx, finishInx)] = randomPoints 1 (length spaces - 1) (mkStdGen seed)
  in ((!!) spaces startInx, (!!) spaces finishInx)
