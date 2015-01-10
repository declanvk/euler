--Project Euler Problem 7 -- 10001st prime

answer :: Integer
answer = last $ take 10002 primes

data Wheel = Wheel Integer [Integer] 

primes :: [Integer]
primes = small ++ large
    where
    	1:p:candidates = roll $ mkWheel small
    	small          = [2,3,5,7,11,13,15,19,17,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523]
    	large          = p : filter isPrime candidates
    	isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) large
    	divides n p    = n `mod` p == 0
    	roll (Wheel n rs) = [n*k+r | k <- [0..], r <- rs]
    	nextSize (Wheel n rs) p = Wheel (p*n) [r' | k <- [0..(p-1)], r <- rs, let r' = n*k+r, r' `mod` p /= 0]
    	mkWheel ds = foldl nextSize w0 ds
    	w0 = Wheel 1 [1]