import Prelude hiding ((&&), gcd)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _ = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse False _ b = b

daysInAMonth :: Int -> Int 
daysInAMonth 1  = 31
daysInAMonth 2  = 28
daysInAMonth 3  = 31
daysInAMonth 4  = 30
daysInAMonth 5  = 31
daysInAMonth 6  = 30
daysInAMonth 7  = 31
daysInAMonth 8  = 31
daysInAMonth 9  = 30
daysInAMonth 10 = 31
daysInAMonth 11 = 30
daysInAMonth 12 = 31
daysInAMonth _  = error ("Invalid month entered")

validDate :: Int -> Int -> Bool
validDate day month = day <= count && day > 0
    where count = daysInAMonth month

--Recursion
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = (n * n) + sumSquares (n - 1)

power :: Int -> Int -> Int
power _ 0 = 1
power n 1 = n
power n p = n * power n (p - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo a b = if a >= b then a else a + sumFromTo (a + 1) b

gcd :: Int -> Int -> Int
gcd a b = if a == b then a else gcd (abs (a - b)) (min a b)

intSquareRoot :: Int -> Int
intSquareRoot n = isr n
    where 
        isr count = if count * count <= n then count else isr (count - 1)

