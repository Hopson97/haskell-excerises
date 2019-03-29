{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit

--addUp = foldr (+) 0 
--myConcat = foldr (++) []


mult10 :: [Int] -> [Int]
mult10 l = map (*10) l

onlyLowerCase :: [Char] -> [Char]
onlyLowerCase l = filter isLower l

--orAll :: [Bool] -> [Bool]
--orAll l = fold (||) l

sumSquares :: [Int] -> Int
sumSquares l = sum (map (^) l)