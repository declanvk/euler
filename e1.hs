--Project Euler Problem 1 -- Multiples of 3 and 5

answer :: Integer
answer = sum $ filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) [1..999]