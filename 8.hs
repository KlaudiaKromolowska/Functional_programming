factors n = filter (isFactor n) [2..n-1]
isFactor n m = n `mod` m == 0
isPrime n = null $ factors n
goldbach n = head $
	filter (\(x,y) -> isPrime x && isPrime y) $
	map (\e -> (e, n - e)) [3..n `div` 2]