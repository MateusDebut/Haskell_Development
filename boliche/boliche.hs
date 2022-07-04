--Desenvolvido por Mateus dos Santos Ribeiro - N°USP: 11796997

-- Tive problemas com a conversão da entrada pelo teclado =(
-- Mas se alterar a lista dá para conferir que o resto está funcionando bem

main = do
    let listaDePontos = [1,4,4,5,6,4,5,5,10,0,1,7,3,6,4,10,2,8,6] --Alterar a lista para testar outra entrada
    print $ imprimePontosPorRodada listaDePontos 
    print $ sum $ calculaPontos listaDePontos

verificaSpare :: Int -> [Int] -> Bool
verificaSpare elemento [] = False
verificaSpare elemento (cabeca:calda) = if elemento + cabeca == 10
                        then True
                        else False

extraiBonus :: [Int] -> [Int]
extraiBonus [] = []
extraiBonus [a] = [a]
extraiBonus (pri:seg:ter:calda) = [pri + seg + ter]
extraiBonus (pri:seg:calda) = [pri + seg]


calculaPontos :: [Int] -> [Int]
calculaPontos [] = []
calculaPontos [x] = [x]
calculaPontos (pri:seg:calda) = if verificaUltimosLances(pri:seg:calda)
                                    then [pri + seg] ++ calculaPontos calda
                                    else 
                                        if pri >= 10
                                        then extraiBonus (pri:seg:calda) ++ calculaPontos (seg:calda)
                                        else if (verificaSpare pri (seg:calda))
                                            then extraiBonus (pri:seg:calda) ++ calculaPontos calda
                                            else [pri + seg] ++ calculaPontos calda

verificaUltimosLances :: [Int] -> Bool
verificaUltimosLances [] = False
verificaUltimosLances [a] = False
verificaUltimosLances [a,b] = False
verificaUltimosLances (pri:seg:ter:calda) = if calda == []
                                                then True
                                                else False

imprimePontosPorRodada :: [Int] -> String
imprimePontosPorRodada [] = ""
imprimePontosPorRodada [a] = if a == 10
                             then "X_|"
                             else show a ++ "|"
imprimePontosPorRodada (pri:seg:calda) = if verificaUltimosLances(pri:seg:calda)
                                         then extraiPontuacaoDaRodada (pri:seg:calda) ++ extraiPontuacaoDaRodada calda ++ "|"
                                         else 
                                            if pri == 10
                                            then extraiPontuacaoDaRodada (pri:seg:calda) ++ "_|" ++ imprimePontosPorRodada (seg:calda)
                                            else extraiPontuacaoDaRodada (pri:seg:calda) ++ "|" ++ imprimePontosPorRodada calda

extraiPontuacaoDaRodada :: [Int] -> String
extraiPontuacaoDaRodada [] = ""
extraiPontuacaoDaRodada [a] = if a == 10
                             then "X"
                             else show a
extraiPontuacaoDaRodada (pri:seg:calda) = if pri == 10
                                            then "X"
                                            else if (verificaSpare pri (seg:calda))
                                                then show pri ++ "/"
                                                else show pri ++ " " ++ show seg

