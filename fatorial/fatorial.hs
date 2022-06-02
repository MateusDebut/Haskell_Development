main = do
    print (fatorial 5)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial a = a * (fatorial (a-1))