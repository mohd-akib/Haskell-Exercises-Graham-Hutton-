data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 1
height (Node l r) = height l + height r

balanced :: Tree a -> Bool
balanced (Leaf a ) = True
balanced (Node l r) = abs (height l - height r) <= 1 