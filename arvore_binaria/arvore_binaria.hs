--Uma árvore binária pode ser definida como:
--Um nó vazio, ou
--Um nó com um valor e dois filhos (sendo que os filhos também são arvores)
data ArvoreBinaria valor = NodeVazio | Node valor (ArvoreBinaria valor)  (ArvoreBinaria valor) deriving (Show, Eq, Ord)

main = do
    let b = NodeVazio
    let b1 = Node 5 NodeVazio b
    let b2 = Node 10 b1 NodeVazio

    putStrLn $ show b2
    putStrLn $ show $ buscaEmArvore b2 7

f :: (Ord valor) => valor -> valor -> Integer
f x y = if x > y
        then 1
        else 0

buscaEmArvore :: (Ord valor) => ArvoreBinaria valor -> valor -> Bool
buscaEmArvore NodeVazio _ = False
buscaEmArvore (Node elementoDaRaiz esquerda direita) elementoBuscado = if elementoDaRaiz == elementoBuscado
                                                                       then True
                                                                       else if elementoDaRaiz > elementoBuscado
                                                                            then buscaEmArvore direita elementoBuscado
                                                                            else buscaEmArvore esquerda elementoBuscado

adicionaElementoEmArvore :: (Ord valor) => ArvoreBinaria valor -> valor -> ArvoreBinaria valor
adicionaElementoEmArvore NodeVazio elemento = Node elemento NodeVazio NodeVazio
adicionaElementoEmArvore (Node elementoDaRaiz esquerda direita) elemento = if elementoDaRaiz == elemento
                                                                           then Node elementoDaRaiz esquerda direita
                                                                           else if elemento > elementoDaRaiz
                                                                                then Node elementoDaRaiz esquerda (adicionaElementoEmArvore direita elemento)
                                                                                else Node elementoDaRaiz direita (adicionaElementoEmArvore esquerda elemento)