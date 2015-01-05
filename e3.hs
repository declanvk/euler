--Project Euler Problem 3 -- Largest prime factor

answer :: Integer
answer = maximum $ primeFactors 600851475143

primeFactors :: Integral t => t -> [t]
primeFactors 1 = []
primeFactors x =
        let res = head (filter (\ v -> gcd x v == v) [2..x])
        in res:primeFactors(x `div` res)