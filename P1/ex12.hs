concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : concatena xs ys

main :: IO ()
main = do
    print(concatena [1, 2, 3] [7, 8])
    
-- concatena [1, 2, 3] [7, 8]
-- concatena [1]:concatena [2, 3] [7, 8]
-- concatena [1, 2]:concatena [3] [7, 8]
-- concatena [1, 2, 3]:concatena [] [7, 8]
-- concatena [1, 2, 3, 7, 8]

