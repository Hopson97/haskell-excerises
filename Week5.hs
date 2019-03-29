{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

--Ex 1: Write function to add one to head of a list 
headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:_) = x + 1

--Ex 2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x : x : xs

--Ex 3
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = head xs : x : tail xs

--Ex 4
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

--Ex 5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

--Ex 6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

--Ex 7
countElems :: Int -> [Int] -> Int
countElems _ [] = 0
countElems n (x:xs) = if x == n then 1 + countElems n xs else countElems n xs

--Ex 8
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x:xs) = if x /= n then x : removeAll n xs else removeAll n xs

--Ex 9
type StudentMark = (String, Int)
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((sname, smark):xs) = if name == sname then smark : listMarks name xs else listMarks name xs

--Ex 10
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (x:xs) (y:ys) = x == y && prefix xs ys

--Ex 11
--subSequence :: [Int] -> [Int] -> Bool
--subSequence [] _ = True