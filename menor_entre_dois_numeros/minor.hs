main = do
    print (retornaMenorValor 12 7)

-- função que mostra o menor valor entre dois inteiros
retornaMenorValor :: Int -> Int -> Int
retornaMenorValor a b
    | a <= b = a
    | otherwise = b