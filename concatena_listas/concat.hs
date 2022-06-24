main = do

    print $ concatenaListas [2,4,6] [1,3,5]

concatenaListas :: [a] -> [a] -> [a]
concatenaListas [] lista = lista
concatenaListas (h:t) lista = h:(concatenaListas t lista)