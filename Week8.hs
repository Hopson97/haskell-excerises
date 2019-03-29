
data Month =    January |
                February |
                March |
                April |
                May |
                June |
                July |
                August |
                September |
                October |
                November |
                December
                deriving Eq

data Season =   Spring |
                Summer |
                Autumn |
                Winter 

data Point = Point Float Float deriving (Eq, Show)

data PositionedShape =  Circle Float Point |
                        Rectangle Float Float Point deriving (Eq, Show)

-- Binary tree algebraic type
data Tree = Null | 
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree :: Tree
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

season :: Month -> Season
season month 
    |   month == March      || month == April       || month == May         = Spring
    |   month == June       || month == July        || month == August      = Summer
    |   month == September  || month == October     || month == November    = Autumn
    |   otherwise     = Winter

numberOfDays :: Month -> Int -> Int
numberOfDays January    _   = 31
numberOfDays February   y   = if mod y 4 == 0 then 29 else 28
--etc

move :: PositionedShape -> Float -> Float -> PositionedShape
move (Circle r p) dx dy = Circle r (offset p dx dy)
move (Rectangle w h p) dx dy = Rectangle w h (offset p dx dy)

offset :: Point -> Float -> Float -> Point
offset (Point x y) dx dy = Point (x + dx) (y + dy) 

