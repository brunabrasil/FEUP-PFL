-- cap4
sortByCont :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCont [] _ = []
sortByCont (x:xs) f =
  | f x 
