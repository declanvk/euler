--Project Euler Problem 5 -- Smallest Multiple

import Data.List (groupBy, sort, group)
import Control.Applicative

data Factor = Factor Int Int deriving (Show, Eq, Ord)

answer :: Int
answer = product . map (\(Factor a b) -> a ^ b) $ map last . groupBy (\(Factor a b) (Factor c d) -> a == c) . sort . concat . map factorMult $ map primeFactors [1..20]

factorMult :: [Int] -> [Factor]
factorMult xs = factorMult' $ group xs where
	factorMult' [] = []
	factorMult' (y:ys) = Factor (head y) (length y) : factorMult' ys

primeFactors :: Integral t => t -> [t]
primeFactors 1 = []
primeFactors x =
        let res = head (filter (\ v -> gcd x v == v) [2..x])
        in res:primeFactors(x `div` res)