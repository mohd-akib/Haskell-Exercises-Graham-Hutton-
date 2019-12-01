(||) :: Bool -> Bool -> Bool

a || b | a==False , b==False = False
       | otherwise =True
