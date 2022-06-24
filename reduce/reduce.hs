main = do
    print (reduce (+) 0 [1..10])


reduce :: (a -> b -> b) -> b -> [a] -> b
reduce _ base [] = base
reduce operacao base (h:t) = operacao h (reduce operacao base t)