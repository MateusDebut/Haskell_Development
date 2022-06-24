main = do
    print $ filtra ( < 10) lista


lista = [3,4,6,9,7,12]

--rerece uma função que vai de um tipo 'a' em um booleano, depois recebe uma lista de 'a' e retorna outra lista de 'a'
--ou seja, se passarmos uma lista de inteiros e uma função que para cada inteiro me dá um true ou false, a função filtra
--retornará a lista de elementos que foram avaliados com true
filtra :: (a -> Bool) -> [a] -> [a]
filtra funcao [] = []
filtra funcao (h:t)
       | funcao h = h:(filtra funcao t)
       | otherwise = filtra funcao t