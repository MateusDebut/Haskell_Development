--Desenvolvido por Mateus dos Santos Ribeiro - N°USP: 11796997

--Está dando erro na extração do bonus do Strike. O algoritmo só está considerando
--O 1º bonus e não está considerando o segundo. Preciso fazer um Slice e pegar um array com os 4 elementos
--Após o strike

main = do
    print $  calculaPontos listaDePontos
    print $ sum $ calculaPontos listaDePontos


listaDePontos = [1,4,4,5,6,4,5,5,10,0,1,7,3,6,4,10,2,8,6]

verificaSpare :: Int -> [Int] -> Bool
verificaSpare a [] = False
verificaSpare a (h:t) = if a + h == 10
                                then True
                                else False

extraiBonusStrike :: [Int] -> Int
extraiBonusStrike [] = 0
extraiBonusStrike [a] = a
extraiBonusStrike (pri:seg:calda) = (pri * 2) + (seg * 2)

extraiBonusSpare :: [Int] -> Int
extraiBonusSpare [] = 0
extraiBonusSpare [a] = a
extraiBonusSpare (pri:seg:calda) = (pri * 2) + seg

calculaPontos :: [Int] -> [Int]
calculaPontos [] = []
calculaPontos [x] = [x]
calculaPontos (pri:seg:calda) = if pri == 10
                                then [pri] ++ [extraiBonusStrike (seg:calda)] ++ calculaPontos calda
                                else if (verificaSpare pri (seg:calda))
                                    then [pri + seg] ++ [extraiBonusSpare calda] ++ calculaPontos calda
                                    else [pri + seg] ++ calculaPontos calda