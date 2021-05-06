--desvio padrao
--desvio_padrao [a] = foldr 

--Media
media :: Fractional p => p -> [p] -> p
media n [] = 0
media n a = sum a / n

somatoriodp :: Floating t => t -> [t] -> t
somatoriodp media [] = 0
somatoriodp media (x:xs) = (x - media)**2 + somatoriodp media xs

desviopadrao :: Floating a => a -> a -> [a] -> a
desviopadrao n m a = sqrt(somatoriodp m a/(n-1))



--a = [983, 992, 991, 990, 980, 994, 998, 995, 981, 983, 994, 997, 985, 982, 996, 995, 989, 988, 997, 986, 990, 993, 991, 983]


--main :: IO ()
--main = do   
    
