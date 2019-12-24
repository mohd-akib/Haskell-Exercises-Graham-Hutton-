adder :: IO ()
adder =  do
           putStr "How many numbers? "
           n <- getLine
           nums <- getnums (read n :: Int)
           putStrLn ("Their total is: " ++ show (sum nums))
         

getnums    :: Int -> IO [Int]
getnums 0   =  return []
getnums n   =  do
                 cs <- getLine
                 let num = read cs :: Int
                 nums <- getnums (n-1)
                 return (num:nums)
