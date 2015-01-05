--Project Euler Problem 2 -- Even Fibonacci Numbers

import Data.List (unfoldr)

answer :: Integer
answer = sum $ filter even $ takeWhile (<4000000) fibonacci

fibonacci :: [Integer]
fibonacci = [0,1] ++ unfoldr (\[a2,a1]-> Just(a2+a1, [a1,a2+a1])) [0,1]