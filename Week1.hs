--1: Function to multiply argument by 10
timesTen :: Int -> Int
timesTen x = x * 10

--2: Function to give sum of three integers
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

--3: Function to give area of circle
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r * r

--4: Function to give volume of cylinder
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder len r = areaOfCircle r * len

--5: Function to give distance between two points
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2) ** 2 + (x1 - x2) ** 2)

--6: Function that returns true if and only if all three numbers are different
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

--7: Function to test if x is divisble by y
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = mod x y == 0

--8: Function to test if a number is even  
isEven :: Int -> Bool
isEven x = mod x 2 == 0

--9: Function to give the average of three numbers
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3

--10: Function to give the absolute (positive value) of a number
absolute :: Int -> Int
absolute x = if x < 0 then -x else x
