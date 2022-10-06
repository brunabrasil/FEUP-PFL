--Learning Haskell by Solving Problems
--week 1
--cap1
nand :: Bool -> Bool -> Bool
nand x y = not(x && y)

funcX :: Floating a => a -> a -> a -> a -> a
funcX x a b c =
  a*t^2 + b*t + c
  where t = cos x + sin x

half :: Fractional a => a -> a
half x = x/2

xor :: Bool -> Bool -> Bool
xor x y = (x && (not y)) || ((not x) && y)

cbrt :: Floating a => a -> a
cbrt x = x**(1/3) -- ** because the exponent is non-integer.

heron :: Floating a => a -> a -> a -> a
heron a b c =
  sqrt(s*(s-a)*(s-b)*(s-c))
  where s=(a+b+c)/2

isTriangular :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangular a b c = (a<=b+c) && (b<=a+c) && (c<=b+a)

isPythagorean :: (Num a, Eq a) => a -> a -> a -> Bool
isPythagorean a b c = (a^2+b^2 == c^2) || (c^2+b^2 == b^2) || (c^2+b^2 == a^2)

f :: (Ord a, Num a, Integral b) => a -> b
f 0 = 0
f x = if x > 0 then 1 else -1

--cap 2
mySwap :: (b, a) -> (a, b)
mySwap (x,y) = (y,x)

distance2 :: Floating a => (a, a) -> (a, a) -> a
distance2 (x,y) (a,b) = sqrt((x-a)^2 + (y-b)^2)

f' :: [a] -> (a,[a])
f' l = ( l !! 2,drop 3 l)

evaluateLength :: [a] -> String
evaluateLength l
  | length l <= 1 = "short"
  | length l <= 3 = "medium-sized"
  | otherwise = "long"

evaluateLength' :: [a] -> String
evaluateLength' [] = "short"
evaluateLength' [_] = "short"
evaluateLength' [_,_] = "medium-sized"
evaluateLength' [_,_,_] = "medium-sized"
evaluateLength' _ = "long"
