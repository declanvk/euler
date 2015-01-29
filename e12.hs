--Project Euler Problem 12 -- Highly divisible triangular number

import Data.List (sort)

--answer :: Int
--answer = 

triangularNumbers :: [Integer]
triangularNumbers = map triangularNumber [1..]
		where triangularNumber x = sum [1..x]

factors :: Integral a => a -> [a]
factors x = factors' x (x `div` 2)
		where factors' x lim
				| comp == GT && ((lim * 2) `mod` x == 0) = ((lim * 2) `div` x):x:(factors' (x - 1) lim)
				| comp == GT && ((lim * 2) `mod` x /= 0) = factors' (x - 1) lim
				| otherwise = []
				where comp = (x `compare` lim)

factors2 x o
		| (realToFrac x) < (sqrt o) = []
		| o `mod` x == 0 = x:(o `div` x):(factors2 (x - 1) o)
		| o `mod` x /= 0 = (factors2 (x - 1) o)