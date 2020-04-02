-- Zadanie 2
main = do
 line <- getLine
 print(czyPalindrom(line))
 main
 
czyPalindrom :: (Eq a) => [a] -> Bool
czyPalindrom xs = xs == (reverse xs)



{-
--Zadanie 2 jako jedna linia do wpisania
czyPalindrom xs = xs == (reverse xs)
-}