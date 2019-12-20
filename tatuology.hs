data Prop = Var Char | Not Prop | And Prop Prop| Imply Prop Prop  deriving Show



find ::Eq k => k -> [(k,v)]-> v
find k' list = head[ v |  (k,v)<-list, k==k']


type Subst = [(Char,Bool)]

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) res ++ map (True:) res
          where res=bools (n-1)


rmdup :: Eq a=> [a]->[a]
rmdup [] = []
rmdup (a:as) = a:filter(/=a) (rmdup as)

vars :: Prop -> [Char]
vars (Var val) = [val]
vars (Not p)   = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) where vs = rmdup(vars p)


eval :: Subst -> Prop -> Bool
eval s (Var c)     = find c s  
eval s (Not p)     = not(eval s p) 
eval s (And p q)   = (eval s p) && (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)
eval s (Or p q)    = (eval p == True) || (eval q == True))
eval s (eq p q)    = (eval p == eval q)


isTaut :: Prop -> Bool
isTaut p = and[eval subs p | subs <- substs p]