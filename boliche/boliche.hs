--Desenvolvido por Mateus dos Santos Ribeiro - N°USP: 11796997

--Está dando erro na extração do bonus do Strike. O algoritmo só está considerando
--O 1º bonus e não está considerando o segundo. Preciso fazer um Slice e pegar um array com os 4 elementos
--Após o strike

main = do
    print $  calculaPontos listaDePontos
    print $ sum $ calculaPontos listaDePontos


listaDePontos = [10,10,10,10,10,10,10,10,10,10,10,10]

verificaSpare :: Int -> [Int] -> Bool
verificaSpare a [] = False
verificaSpare a (h:t) = if a + h == 10
                        then True
                        else False

extraiBonusStrike :: [Int] -> [Int]
extraiBonusStrike [] = []
extraiBonusStrike [a] = [a]
extraiBonusStrike (pri:seg:ter:calda) = [pri + seg + ter]
extraiBonusStrike (pri:seg:calda) = [pri + seg]


extraiBonusSpare :: [Int] -> [Int]
extraiBonusSpare [] = []
extraiBonusSpare [a] = [a]
extraiBonusSpare(pri:seg:ter:calda) = [pri + seg + ter]
extraiBonusSpare (pri:seg:calda) = [pri + seg] 

calculaPontos :: [Int] -> [Int]
calculaPontos [] = []
calculaPontos [x] = [x]
calculaPontos (pri:seg:calda) = if verificaUltimosLances(pri:seg:calda)
                                    then [pri + seg] ++ calculaPontos calda
                                    else 
                                        if pri >= 10
                                        then extraiBonusStrike (pri:seg:calda) ++ calculaPontos (seg:calda)
                                        else if (verificaSpare pri (seg:calda))
                                            then extraiBonusSpare (pri:seg:calda) ++ calculaPontos calda
                                            else [pri + seg] ++ calculaPontos calda

verificaUltimosLances :: [Int] -> Bool
verificaUltimosLances [] = False
verificaUltimosLances [a] = False
verificaUltimosLances [a,b] = False
verificaUltimosLances (pri:seg:ter:calda) = if calda == []
                                                then True
                                                else False