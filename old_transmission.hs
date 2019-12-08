import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits =  sum[w*b | (w,b) <- zip weights bits]
                where weights = iterate (*2) 1


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin(n `div` 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String-> [Bit]
encode = concat.map(make8.int2bin.ord)


countOnes :: [Int]->Int
countOnes [] = 0
countOnes (x:xs) = if x==1 then 1+countOnes(xs) else countOnes(xs) 

addParityBit :: [Bit] -> [Bit]
addParityBit x | ((countOnes x)  `mod` 2) == 0 = x ++ [0]
               | otherwise = x ++ [1]

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits:chop8 (drop 8 bits)

extractParityBit :: [Int] ->  Int
extractParityBit x = (head(reverse x))

checkParityBit :: [Bit] -> Bool
checkParityBit x = (extractParityBit x) == (countOnes (init x ) `mod` 2)

checkAndDropParityBit :: [Bit] -> [Bit]
checkAndDropParityBit x = if checkParityBit x then tail x
                          else error "Detected Wrong Parity Bit Can't Proceed"

decode :: [Bit]->String
decode = map(chr.bin2int).chop8


channel :: [Bit]->[Bit]
channel = id

transmit :: String -> String
transmit = decode.channel.encode

