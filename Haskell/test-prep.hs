import Data.List
import Data.Function

{-
Задача 2. Да се напише на Haskell функция sumUnique :: [[Int]] -> Int, която по списък
от списъци от цели числа намира сумата на тези от числата, които са уникални в рамките на
списъка, в който се срещат.
Примери:
sumUnique [[1,2,3,2],[-4,-4],[5]] → 9 (= 1+3+5)
sumUnique [[2,2,2],[3,3,3],[4,4,4]] → 0
sumUnique [[1,2,3],[4,5,6],[7,8,9]] → 45
-}
isUnique :: Int -> [Int] -> Bool
isUnique n xs = 1 == length[x | x <- xs, x == n]

sumUnique :: [[Int]] -> Int
sumUnique xss = sum (map sum (map (\xs -> [x | x <- xs, isUnique x xs]) xss))

{-
Задача 3. Продукт се представя с наредена двойка от вида (име, цена). Наличността в даден
магазин се представя със списък от продукти.
type Product = (String,Double)
type StoreAvailability = [Product]
а) Да се напише на Haskell функция
closestToAverage :: StoreAvailability -> String, която намира името на продукта,
чиято цена е най-близка до средната цена за всички продукти. Ако има повече от един такъв
продукт, функцията да връща името на кой да е от намерените.
б) Да се напише на Haskell функция
cheaperAlternative :: StoreAvailability -> Int, която намира броя на продуктите,
за които има продукт със същото име, но по-ниска цена.
Примери:
store1=[(”bread”,1),(”milk”,2.5),(”lamb”,10),(”cheese”,5),(”butter”,2.3)]
closestToAverage store1 → ”cheese”
store2=[(”bread”,1),(”cheese”,2.5),(”bread”,1),(”cheese”,5),(”butter”,2.3)]
cheaperAlternative store2 → 1
-}
type Product = (String,Double)
type StoreAvailability = [Product]
--намира средната цена на продуктите
average :: StoreAvailability -> Double
average ps = (sum (map (\p -> snd p) ps)) / fromIntegral (length ps)
--връща списък от продукти с втори елементи разликата по абсолютна стойност на цената на всеки продукт и средната цена на продуктите
d :: [Product] -> StoreAvailability
d ps = map (\p -> (fst p, abs(snd p - averageValue))) ps where
  averageValue = average ps
--closestToAverage
closestToAverage :: StoreAvailability -> String
closestToAverage ps = fst (head (sortBy (compare `on` snd) (d ps)))
--unfinished
cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative ps = undefined
f :: StoreAvailability -> [[Product]]
f ps = map (\p -> filter (\x -> fst p == fst x) ps) ps

{-
Задача 4. Нека е даден списък от точки в тримерно пространство, представен като списък от
наредени тройки. Да се напише на Haskell функция
minDistance :: [(Double,Double,Double)] -> Double, която намира най-малкото от
разстоянията между двойките точки от списъка.
Разстоянието d се дeфинира по следния начин: ако разглеждаме точките p1=(x1, y1, z1) и
p2=(x2, y2, z2), то d(p1, p2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2).
-}
--distance
dist :: (Double, Double, Double) -> (Double, Double, Double) -> Double
dist p1@(x1, y1, z1) p2@(x2, y2, z2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2)
-- allDistances
allDistances:: [(Double, Double, Double)] -> [Double]
allDistances ps = concatMap (\p -> (map (\other -> dist p other) (tail ps))) ps

--minDistance
minDistance :: [(Double, Double, Double)] -> Double
minDistance tuples = minimum $ allDistances tuples

{-
Задача 5. Да се дефинира функция
maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a), за която оценката на
обръщението maximize l, където l е непразен списък от едноместни числови функции, да е
едноместна числова функция на аргумент x, която дава стойността f(x) на тази фунция f от
списъка l, за която числото f(x) е най-голямо по абсолютна стойност.
Пример:
Ако fn = maximize [(\x -> x*x*x),(\x -> x+1)],
то fn 0.5 → 1.5, а fn (-2) → -8
-}
-- \x -> fst (head (sortBy (compare `on` snd)
maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fs = \x -> fst (last (sortBy (compare `on` snd) [(f, fx) | f <- fs, fx <- (map (\fun -> abs (fun x)) fs)])) x


{-
Задача 6. Функцията g е обратна на функцията f в дадено множество А, ако f . g = id в A и g . f =
id в A. Да се напише на езика Haskell функция
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool, която за
дадени целочислени функции f и g връща True точно когато g е обратна на f в даден
целочислен интервал [a, b].
Примери:
inverseFun (\x -> x+1) (\x -> x-1) 5 10 → True
inverseFun (\x -> x*x) (\x -> x^3) 0 1 → True
inverseFun (\x -> x+1) (\x -> x+2) 0 1 → False
-}
--compose
compose :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
compose f g = \x -> f (g x)
--inverseFun 
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = length (filter (\x -> compose f g x == x && compose g f x == x) xs) == length xs where
  xs = [a..b]
  
{-
Задача 7. Да се напише на езика Haskell функция mirrorBst :: BTree а -> BTree а,
която получава двоично двоично дърво bt и го преобразува в “огледалното” му такова bt’.
-}
data BTree a = Empty | Node a (BTree a) (BTree a)
mirrorBst :: BTree а -> BTree а
mirrorBst Empty = Empty
mirrorBst (Node v Empty Empty) = (Node v Empty Empty)
mirrorBst (Node v left Empty)  = (Node v Empty (mirrorBst left))
mirrorBst (Node v Empty right) = (Node v (mirrorBst right) Empty)
mirrorBst (Node v left right)  = (Node v (mirrorBst right) (mirrorBst left))
{-
функцията getLevels :: BTree a -> [(a,Int)], която получава двоично дърво и
връща списък от двойки, представящи всеки от възлите на дървото със стойността му и
съоветното му ниво в това дърво;
-}
getLevels :: BTree a -> [(a,Int)]
getLevels t@(Node v left right)  = helper t 0 where
  helper Empty _ = []
  helper t@(Node v l r) counter = sortBy (compare `on` snd) ([(v, counter)] ++ (helper l (counter + 1)) ++ (helper r (counter + 1)))
{-
функцията inorder :: BTree a -> [a], която получава двоично дърво и връща
обхождането му в ред Ляво-Корен-Дясно;
-}
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node v Empty Empty) = [v]
inorder t@(Node v left right) = inorder left ++ [v] ++ inorder right
main :: IO()
main = let 
    store1 = [("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)]
    fn = maximize [(\x -> x*x*x),(\x -> x+1)]
    tree = (Node 1 
              (Node 2 
                  (Node 5 Empty Empty)
                  Empty)
              (Node 3 
                  (Node 7 Empty Empty) 
                  (Node 6 Empty Empty)))
    in do
        -- Задача 3a.
        print $ closestToAverage store1
        -- Задача 5
        print $ fn 0.5
        -- Задача 7
        print $ getLevels tree
        print $ inorder tree
        print $ getLevels (mirrorBst tree)
        print $ inorder (mirrorBst tree) 