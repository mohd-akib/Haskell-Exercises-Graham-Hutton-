bubble :: Ord a  => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
  | x > y     = x:bubble(y:xs)
  | otherwise = y:bubble(x:xs)


sort :: Ord a => [a] -> [a]
sort []  = []
sort [x] = [x] 
sort xs  = last (sorted) : sort(init(sorted))
           where sorted = bubble xs

