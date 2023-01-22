maxpos :: [Int] -> Int
maxpos [x]
  | x >= 0 = x
  | otherwise = 0
maxpos (x:xs)
  | x > head xs = maxpos (x:(tail xs))
  | otherwise = maxpos xs

dups :: [a] -> [a]
dups l = dupsAux l 1

dupsAux :: [a] -> Int -> [a]
dupsAux [] _ = []
dupsAux (x:xs) n
  | odd n = x:x:(dupsAux xs (n+1))
  | otherwise = x:(dupsAux xs (n+1))

transforma :: String -> String
transforma [] = []
transforma (x:xs)
  | x=='a' ||  x=='e' ||  x=='i' ||  x=='o' || x=='u' = x:'p':x:(transforma xs)
  | otherwise = x:transforma xs

type Vector = [Int]
type Matriz = [[Int]]

transposta :: Matriz -> Matriz
transposta [] = []
transposta l = [head x | x <- l] : transposta [tail x | x <- l, tail x /= []]

prodInterno :: Vector -> Vector -> Int
prodInterno l1 l2 = sum [ x*y | (x,y) <- zip l1 l2]

prodMat :: Matriz -> Matriz -> Matriz
prodMat [] [] = []
prodMat l1 l2 = [[prodInterno x y | x <- transposta (l2)] | y <- l1]

data Arv a = F | N a (Arv a) (Arv a) deriving(Show)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a b c) = N (alturasAux (N a b c)) (alturas b) (alturas c)

alturasAux :: Arv a -> Int
alturasAux F = 0
alturasAux (N a b c) = 1 + max (alturasAux b) (alturasAux c)


equilibrada :: Arv a -> Bool
equilibrada F = 0
equilibrada (No a b c) 
