--Learning Haskell by Solving Problems
--week 4
--LI-10
myCycle :: [a] -> [a]
myCycle l = l ++ myCycle l

--LI-34
dropN :: [a] -> Int -> [a]
dropN l n = [ x | (x,y) <- zip l [1..], y `mod` n /= 0]

--voltar aqui dps

--HO-19
myIterate :: (a -> a) -> a -> [a]
myIterate f n = n:[ f x | x <- myIterate f n]


type Pair a = (a,a)
type Relation a = [Pair a]

isReflexive :: Eq a -> Relation a -> Bool
isReflexive r = isReflexiveAux r r[a]

isReflexiveAux :: (Eq a) => [Pair a] -> Relation a -> Bool
isReflexiveAux [] _ = True
isReflexiveAux ((x,y):xs) r
  | (y,x) `elem` r = isReflexiveAux xs r
  | otherwise = False

data Shape = Circle Double Double Double | Rectangle Double Double
--UT-6
perimeter :: Shape -> Double
perimeter (Circle _ _ r) = 2*pi*r
perimeter (Rectangle x1 y1 x2 y2) = 2*abs(x2-x1) + 2*abs(y2-y1)

--UT-7
type HashMap k v = [(k,v)]
lookup :: (Eq k) => k -> HashMap k v -> Maybe v
lookup _ [] = Nothing
lookup a ((k,v):xs)
  | a == k = Just v
  | otherwise = lookup a xs

--UT-8
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ (flatten (List xs))

data Country = Ct { name :: String, population :: Int, area :: Double, continent :: String}

populationDensity :: Country -> Double
populationDensity p = fromIntegral (population(c) / area(c))
