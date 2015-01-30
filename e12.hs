--Project Euler Problem 12 -- Highly divisible triangular number

--INEFFICIENT

answer :: Integer
answer = (fst . head) $ take 1 $ dropWhile (\(_,x) -> x < 500) $ map (\x -> (x, length $ factors x x ((sqrt (fromIntegral x :: Double)) :: Double))) triangularNumbers

triangularNumbers :: [Integer]
triangularNumbers = scanl1 (+) [1..]
		
factors :: Integer -> Integer -> Double -> [Integer]
factors x orig lim
	| comp == GT && (orig `mod` x == 0) = (orig `div` x):x:(factors (x - 1) orig lim)
	| comp == GT && (orig `mod` x /= 0) = factors (x - 1) orig lim
	| otherwise = []
	where comp = (((fromIntegral x) :: Double) `compare` lim)