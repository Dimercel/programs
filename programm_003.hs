#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

-- Реализация волнового алгоритма Ли (https://en.wikipedia.org/wiki/Lee_algorithm)
import Data.List
import Data.Set (fromList, member)
import System.Random


type Point = (Int, Int)


seed = 42 :: Int



-- СЦЕНА.
--
--   Прежде чем описывать сам алгоритм, расскажем о сцене на которой мы будем его испытывать.
-- Сцена представляет собой квадратную область со множеством пустых мест внитри нее. На каждом
-- свободном месте может быть размещено препятствие не позволяющие проходить через это место.
--   Суть же алгоритма в том, чтобы построить кратчайший путь между двумя произвольными,
-- свободными местами минуя все препятствия. Также возможен вариант, когда ни одного пути между
-- начальным и конечным местами не существует.
--   Теперь же опишем систему координат для однозначной идентификации конкретного места внутри
-- сцены. В нашей системе используются строки и столбцы. Отсчет строк и столбцов начинается с
-- нуля, а заканчивается на позициях определяемых размерами сцены. Координаты места, находящегося
-- в левом верхнем углу сцены - (0,0), в правом верхнем - (0, n), в левом нижнем - (n, 0) и наконец
-- в правом нижнем - (n,n).
--   Итак, с терминологией относительно сцены мы разобрались, теперь самое время рассмотреть
-- поэтапную реализацию построения сцены.

--   Для начала нам потребуется сгенерировать несколько координат мест, где будут размещены
-- препятствия. Стоит отметить что здесь возможны случаи получения замкнутых областей. Таким образом
-- может получиться что путь построить невозможно, если к примеру, начальное место находится внутри
-- такой области, а конечное снаружи.
randomPoints :: Int -> Int -> StdGen -> [Point]
randomPoints 0 _ _ = []
randomPoints count max gen =
  let (first, newGen)   = randomR (0, max) gen
      (second, nextGen) = randomR (0, max) newGen
  in (first, second) : randomPoints (count - 1) max nextGen

--   Далее мы построим ограждение из препятствий вокруг нашей сцены, с целью немного облегчить
-- алгоритм. Так как сцена имеет квадратную форму, сделать это будет относительно просто. Нам
-- понадобится два горизонтальных ряда и два вертикальных, в сумме образующих квадрат размера
-- на единицу больше чем сама сцена. Внутри него и будет заключена сцена.
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

--   Наконец-то мы дошли до построения самой сцены, теперь это выглядит вполне тривиальным
-- действием. Мы просто случайным образом генерируем препятствия внутри сцены указанного размера
-- и строим ограждение из препятствий вокруг нее.
makeScene :: Int -> [Point]
makeScene size
  | size > 1 = createLimits size size ++ randomPoints size (size - 1) (mkStdGen seed)
  | otherwise = makeScene 2

sceneSize :: [Point] -> Int
sceneSize points = maximum (map fst points)

-- Нам пригодится знать какие места в сцене еще не заняты.
freeSpaces :: [Point] -> [Point]
freeSpaces scene =
  let size = sceneSize scene
      spaces = [(row, col) | row <- [0 .. size - 1], col <- [0 .. size - 1]]
      pointsSet = fromList scene
  in foldr (\x acc -> if member x pointsSet then acc else x : acc) [] spaces

--   Наша сцена полностью готова, но нам также необходимо знать на каких местах находятся начальное
-- и конечное положения пути. Их мы также выберем случайным образом, из числа свободных мест сцены.
endPoints :: [Point] -> (Point, Point)
endPoints scene =
  let spaces = freeSpaces scene
      [(startInx, finishInx)] = randomPoints 1 (length spaces - 1) (mkStdGen seed)
  in ((!!) spaces startInx, (!!) spaces finishInx)



-- ВОЛНОВОЙ АЛГОРИТМ ЛИ.
--
-- Приступим собственно к описанию самого алгоритма.

neighbors :: Point -> [Point]
neighbors (row, col) = [
  (row - 1, col),
  (row + 1, col),
  (row, col - 1),
  (row, col + 1)
  ]
