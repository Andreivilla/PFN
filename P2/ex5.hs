--ex5--
converte :: [Int] -> [String]
converte [] = []
converte (x:xs) = show(x):converte xs