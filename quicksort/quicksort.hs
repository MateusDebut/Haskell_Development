main = do

    print $ quicksort [7,65,7,91,4,73]

filtra :: (a -> Bool) -> [a] -> [a]
filtra funcao [] = []
filtra funcao (h:t)
       | funcao h = h:(filtra funcao t)
       | otherwise = filtra funcao t

concatenarListas :: [a] -> [a] -> [a]
concatenarListas [] lista = lista
concatenarListas (h:t) lista = h:(concatenarListas t lista)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] --quicksort de uma lista vazia, é uma lista vazia
quicksort (h:t) = concatenarListas l $ concatenarListas e $ g
    where l = quicksort $ filtra (< h) t --quicksort da cauda filtrada com os menores que o pivô
          e = filtra (== h) (h:t) --a lista toda filtrada com os iguais ao o pivô
          g = quicksort $ filtra (> h) t --quicksort da cauda filtrada com os maiores que o pivô