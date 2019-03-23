--конструира списък от цифрите на число
digits :: Int -> [Int]
digits n
  | n == 0 = []
  | otherwise = digits (n `div` 10) ++ [(n `mod` 10)]
  
--конструира списък от елементите на четна позиция
evenPositions :: [Int] -> [Int]
evenPositions xs = [x | (i, x) <- zip [1..] xs, even i]

--конструира списък от елементите на нечетна позиция
oddPositions :: [Int] -> [Int]
oddPositions xs = [x | (i, x) <- zip [1..] xs, odd i]

--удвоява елементите на четна позиция
doubleEven :: [Int] -> [Int]
doubleEven xs = map (\x -> 2 * x) (evenPositions xs)

--събира цифрите на число
sumDigits :: Int -> Int
sumDigits n 
  | n >= 0 && n<= 9 = n
  | otherwise = n `mod` 10 + sumDigits (n `div` 10)
  

--сумираме 
sumAll :: [Int] -> Int
sumAll xs = sum (oddPositions xs) + sum (map sumDigits (doubleEven xs))

--функцията от условието
calcLuhnChecksum :: Int -> Int
calcLuhnChecksum n = 9 * (sumAll (digits n)) `mod` 10

