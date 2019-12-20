data Tree a = Null | Leaf a | Node (Tree a) a (Tree a) deriving Show  

takeFirstHalf :: [a] -> [a]
takeFirstHalf []     = []
takeFirstHalf l =  take (length l `div` 2) l 

takeSecondHalf :: [a] -> [a]
takeSecondHalf [] = []
takeSecondHalf l  = drop (length l `div` 2) l

convertToTree :: [a] -> Tree a
convertToTree []  = Null 
convertToTree [x] = Leaf x
convertToTree (x:xs) = Node (convertToTree (takeFirstHalf xs) ) x (convertToTree  (takeSecondHalf xs)) 