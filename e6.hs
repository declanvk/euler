--Project Euler Problem 6 -- Sum Square Difference

answer :: Int
answer = ((sum [1..100]) ^ 2) - (sum $ map (^2) [1..100])