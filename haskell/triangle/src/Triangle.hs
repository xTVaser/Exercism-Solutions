module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Eq x, Ord x, Num x) => x -> x -> x -> TriangleType
triangleType side1 side2 side3
    | a > b + c || b > a + c || c > a + b || side1 + side2 + side3 == 0 = Illegal
    | a == b && a == c && b == c = Equilateral
    | a /= b && a /= c && b /= c = Scalene
    | otherwise = Isosceles
    where a = side1
          b = side2
          c = side3

getFirst :: (a, b, c) -> a
getFirst (a,_,_) = a

getSecond :: (a, b, c) -> b
getSecond (_,b,_) = b

getThird :: (a, b, c) -> c
getThird (_,_,c) = c
