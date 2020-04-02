-- Zadanie 3
main = do
 line <- getLine
 print(compress(line))
 main
 
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys


{-
--Zadanie 3 jako jedna linia do wpisania
compress x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x
-}

