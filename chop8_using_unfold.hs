type Bit = Int

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)



int2bin = unfold(== 0)(`mod` 2)(`div` 2)


chop_8 = unfold(null )(take 8 )(drop 8)

map1 f = unfold (null) (f.head) (tail)


