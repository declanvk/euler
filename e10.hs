--Project Euler Problem 10 -- Summation of primes

answer :: Integer
answer = sum $ takeWhile (<2000000) primes

data Wheel = Wheel Integer [Integer] 

primes :: [Integer]
primes = small ++ large
    where
    	1:p:candidates = roll $ mkWheel small
    	small          = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271]
    	large          = p : filter isPrime candidates
    	isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) large
    	divides n p    = n `mod` p == 0
    	roll (Wheel n rs) = [n*k+r | k <- [0..], r <- rs]
    	nextSize (Wheel n rs) p = Wheel (p*n) [r' | k <- [0..(p-1)], r <- rs, let r' = n*k+r, r' `mod` p /= 0]
    	mkWheel ds = foldl nextSize w0 ds
    	w0 = Wheel 1 [1]
