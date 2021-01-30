take :: int -> [a] -> [a]
take _ xs = []
take n [] = []
take n (x:xs) = x : take (n-1) xs