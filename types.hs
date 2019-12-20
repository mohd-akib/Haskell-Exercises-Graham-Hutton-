data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int(Succ n) = 1+nat2int n


data List a = Nil | Cons a (List a) deriving Show
len :: List a -> Int
len Nil  = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occur :: Eq a => a -> Tree a -> Bool
occur x (Leaf y)     = (x==y)
occur x (Node l y r) = (x==y) || occur x l || occur x r

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l y r) = flatten l ++ [y] ++ flatten r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) =  (x==y)
occurs x (Node l y r) | x==y = True
                      | x<y  = occurs x l
                      | otherwise = occur x r

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools(n-1)


data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y ) = value x + value y

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)



