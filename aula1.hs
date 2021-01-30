elemento :: [Int] -> Int -> Int
--caso 1
elemento (x:xs) 0 = x
--caso 2
elemento (x:xs) n = elemento xs (n-1)
--caso 3
elemento [] n = error "erro : lista vazia"



main :: IO()
main = do
    putStrLn "hello world"
    print $ elemento [1,2,3,4,5656,9] 2