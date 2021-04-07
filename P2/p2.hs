--ex1--
primeiro :: (a -> Bool) -> [a] -> Maybe a
primeiro f [] = Nothing 
primeiro f (x:xs) = if f x then Just x else primeiro f xs

--ex4--
data Tree a = Leaf
            | Branch (Tree a) a (Tree a)

atravessar :: (a -> b) -> Tree a -> Tree b
atravessar f Leaf = Leaf
atravessar f (Branch esq a dir) = Branch (atravessar f esq) (f a) (atravessar f dir)

--ex5--
converte :: [Int] -> [String]
converte [] = []
converte (x:xs) = show(x):converte xs



impar :: Integral a => a -> Bool
impar a = if mod a 2 == 1 then True else False 

main :: IO ()
main = do
    print (primeiro impar [2,3,4,6,44,32])