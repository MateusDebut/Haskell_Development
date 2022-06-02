main = do
    print (quadrado 2)

-- Funções são definidas fora da main e chamadas na main. Na linha abaixo estamos definindo a função quadrado
quadrado :: Int -> Int
quadrado x = x * x