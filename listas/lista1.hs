--ex1--
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : concatena xs ys

--ex2--
pertence :: Eq a => a -> [a] -> Bool


main :: IO ()
main = do
    print (concatena [1,2,3,4] [6,45,32])