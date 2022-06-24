main = do
    print (reverseList [4,8,6,9,5])

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
