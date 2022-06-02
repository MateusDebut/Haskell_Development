main = do
    print (tamanhoLista [4,3,5,7,2])

tamanhoLista :: [a] -> Int
tamanhoLista [] = 0
tamanhoLista (_:t) = 1 + tamanhoLista t