podzielniki n = pod2 n 2 
  where
    pod2 n d
      | d*d > n = [n] 
      | n `mod` d == 0 = d : pod2 (n `div` d) d
      | otherwise = pod2 n (d + 1) 
