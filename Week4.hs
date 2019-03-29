import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int -> Int -> (Int, Int)
sumDifference a b = (a + b, a - b)

grade :: StudentMark -> Char
grade (_, mark) 
    |   mark >= 70 = 'A'
    |   mark >= 60 = 'B'
    |   mark >= 50 = 'C'
    |   mark >= 40 = 'D'
    |   otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (name, mark)
    |   mark > 40 = (name, 40)
    |   otherwise = (name, mark)

firstNumbers :: Int -> [Int]
firstNumbers a = [1 .. a]

firstSquares :: Int -> [Int]
firstSquares a = map