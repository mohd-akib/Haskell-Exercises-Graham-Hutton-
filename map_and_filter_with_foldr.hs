f :: a -> [b] -> [c]
f a b = a:b
map1 :: (a->[b]->[c]) -> [a] -> [b]
map1 f a = foldr(f) []
