absolute :: Int -> Int
absolute x 
    |   x < 0 = (-x)
    |   otherwise = x

sign :: Int -> Int
sign x 
    |   x < 0 = -1
    |   x > 0 = 1
    |   otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
 | x==y && y==z = 3
 | x/=y && y/=z && x/=z = 0
 | otherwise = 2


sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diaLength z + diaLength y + diaLength x
    where 
        diaLength w = sqrt (w ** 2 + w ** 2)

taxiFare :: Int -> Float
taxiFare distance = fromIntegral (220 + fare) / 100
    where
        basicFair
            | distance > 10 = 500
            | otherwise = distance * 50
        fare 
            |   distance > 10 = ((distance - 10) * 30) + basicFair
            |   otherwise = basicFair
        

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    |   x > average && y > average && z >  average = 3
    |   x > average && y > average && z <= average = 2
    |   x > average && z > average && y <= average = 2
    |   z > average && y > average && x <= average = 2
    |   x > average && y <= average && z <= average = 1
    |   y > average && y <= average && x <= average = 1
    |   z > average && y <= average && x <= average = 1
    |   otherwise = 0
        where 
            average = div (x + y + z) 3

validDate :: Int -> Int -> Bool 
validDate day month 
    |   day < 0 = False
    |   day <= daysInMonth month 2001 = True
    |   otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
    |   month == 1 = 31
    |   month == 2 = if isLeapYear then 29 else 28
    |   month == 3 = 31
    |   month == 4 = 30
    |   month == 5 = 31
    |   month == 6 = 30
    |   month == 7 = 31
    |   month == 8 = 30
    |   month == 9 = 31
    |   month == 10 = 30
    |   month == 11 = 31
    |   month == 12 = 30
    |   otherwise = error "Month does not exist."
        where
            isLeapYear = (mod  year 4) == 0

--daysInMonth :: Int -> Int -> Int
--daysInMonth month year
--    |   month > 7   = if mod month 2 == 0 then 31 else 30
--    |   month == 2  = if isLeapYear       then 29 else 28
--    |   otherwise   = if mod month 2 == 0 then 30 else 31
--        where
--            isLeapYear = (mod  year 4) == 0