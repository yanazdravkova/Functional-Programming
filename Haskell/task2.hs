import Data.List

-- Забележка* клетка, която не е съсед на никоя жива клетка, е мъртва и няма опция да се съживи на следващата итерация

-- фнукцията neighbours cell връща списък от съседите на клетка cell + самата клетка
neighbours :: (Integer, Integer) -> [(Integer, Integer)]
neighbours cell = helpNeighbours cell where
  helpNeighbours cell = [(x, y) | x <- map (\el -> el + fst(cell)) [-1, 0, 1], y <- map (\el -> el + snd(cell)) [-1, 0, 1]]
  
-- функцията isAlive cell board проверява дали клетка е жива
isAlive :: (Integer, Integer) -> [(Integer, Integer)] -> Bool
isAlive cell board = cell `elem` board

-- функцията aliveNeighbours cell board връща броя на живите съседи на cell
aliveNeighbours :: (Integer, Integer) -> [(Integer, Integer)] -> Int
aliveNeighbours cell board 
  | isAlive cell board = length [n | n <- (neighbours cell), isAlive n board] - 1 
  | otherwise = length [n | n <- (neighbours cell), isAlive n board]

--функция foo приема клетката и съседите й и връща списък само от тези от тях, които са живи на следващата итерация
foo :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
foo [] _ = []
foo (c:cs) board
  | isAlive c board && (aliveNeighbours c board == 2 || aliveNeighbours c board == 3) = c : foo cs board -- продължава да живее
  | not (isAlive c board) && aliveNeighbours c board == 3 = c : foo cs board -- ражда се
  | otherwise = foo cs board -- клетка c умира; сега е някое от следните 1. жива е, но има <2 живи съседа 2. е мъртва и няма достатъчно съседи да я съживят 3. жива е, но има прекалено много съседи и умира 

--функцията removeDuplicates премахва повторенията на елеменети в списък от координати
removeDuplicates :: [(Integer, Integer)] -> [(Integer, Integer)]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs
  
-- функцията withDuplicates прави същото, като gameOfLife, но не връща множество като отговор, а има риск от повторение на елеменети 
-- за всяка жива клетка правим списък на живите на следващата итерация сред нея и съседите й и конкатенираме тези списъци
withDuplicates :: [(Integer, Integer)] -> [(Integer, Integer)]
withDuplicates [] = []
withDuplicates board = help (head board) (tail board) board where
  help _ [] _ = []
  help curr rest board = foo (neighbours curr) board ++ help (head rest) (tail rest) board
  
-- gameOfLife
gameOfLife :: [(Integer, Integer)] -> [(Integer, Integer)]
gameOfLife board = removeDuplicates (withDuplicates board)
