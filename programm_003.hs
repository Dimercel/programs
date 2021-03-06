#!/usr/bin/env stack
-- stack --resolver lts-14.4 script

-- Реализация волнового алгоритма Ли (https://en.wikipedia.org/wiki/Lee_algorithm)
import Data.List
import qualified Data.Set as S
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
-- препятствия. Стоит отметить, что здесь возможны случаи получения замкнутых областей. Таким образом,
-- может получиться что путь построить невозможно, если к примеру, начальное место находится внутри
-- такой области, а конечное снаружи.
randomPoints :: Int -> Int -> StdGen -> S.Set Point
randomPoints count max gen
  | count > 0 && max > 0 =
    let (gen1, gen2) = split gen
    in S.fromList $ zip (take count (randomRs (0, max) gen1)) (take count (randomRs (0, max) gen2))
  | otherwise = S.fromList []

--   Далее мы построим ограждение из препятствий вокруг нашей сцены, с целью немного облегчить
-- алгоритм. Так как сцена имеет квадратную форму, сделать это будет относительно просто. Нам
-- понадобится два горизонтальных ряда и два вертикальных, в сумме образующих квадрат размера
-- на единицу больше чем сама сцена. Внутри него и будет заключена сцена.
createLimits :: Int -> Int -> S.Set Point
createLimits 0 _ = S.fromList []
createLimits _ 0 = S.fromList []
createLimits rowCount colCount =
  let hLimit from to row = [(row, col) | col <- [from .. to]]
      vLimit from to col = [(row, col) | row <- [from .. to]]
  in S.fromList $ concat [
    hLimit (-1) colCount (-1),
    hLimit (-1) colCount rowCount,
    vLimit 0 (rowCount - 1) (-1),
    vLimit 0 (rowCount - 1) colCount
    ]

--   Наконец-то мы дошли до построения самой сцены, теперь это выглядит вполне тривиальным
-- действием. Мы просто случайным образом генерируем препятствия внутри сцены указанного размера
-- и строим ограждение из препятствий вокруг нее.
makeScene :: Int -> S.Set Point
makeScene size
  | size > 1 = S.union (createLimits size size) (randomPoints size (size - 1) (mkStdGen seed))
  | otherwise = makeScene 2

sceneSize :: S.Set Point -> Int
sceneSize points = maximum $ S.toList (S.map fst points)

-- Нам пригодится знать какие места в сцене еще не заняты.
freeSpaces :: S.Set Point -> [Point]
freeSpaces scene =
  let size = sceneSize scene
      spaces = [(row, col) | row <- [0 .. size - 1], col <- [0 .. size - 1]]
  in foldr (\x acc -> if S.member x scene then acc else x : acc) [] spaces

--   Наша сцена полностью готова, но нам также необходимо знать на каких местах находятся начальное
-- и конечное положения пути. Их мы также выберем случайным образом, из числа свободных мест сцены.
endPoints :: S.Set Point -> (Point, Point)
endPoints scene =
  let spaces = freeSpaces scene
      [(startInx, finishInx)] = S.toList $ randomPoints 1 (length spaces - 1) (mkStdGen seed)
  in ((!!) spaces startInx, (!!) spaces finishInx)



-- ВОЛНОВОЙ АЛГОРИТМ ЛИ.
--
--   Приступим собственно к описанию самого алгоритма. На первом этапе мы должны сгенерировать "волну"
-- из начального места выбрав при этом все соседние клетки начального места. "Волна" представляет собой
-- множество соседних мест с предыдущей "волной", отмеченных уровнем. То есть, например для соседних со
-- стартовым местом, уровень будет равен единице, для соседних с уровнем 1, уже будет уровень 2 и т.д.
--   Алгоритм заканчивается, если более пустых мест не осталось или в очередной волне попалось конечное
-- положение.

-- Интересно что соседние клетки можно определить по разному и алгоритм будет по прежнему работать для
-- различных реализаций. В нашем случае все очень просто и соседними клетками являются те клетки, что
-- образуют знак "плюс" с указанным местом. Оно конечно находится в центре.
neighbors :: Point -> S.Set Point
neighbors (row, col) = S.fromList [
  (row - 1, col),
  (row + 1, col),
  (row, col - 1),
  (row, col + 1)
  ]
