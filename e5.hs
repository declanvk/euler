--Project Euler Problem 5 -- Smallest Multiple

--Inefficient, marked incomplete
answer = take 1 $ filter (allDivis [2,3,5,7,9,11,12,13,16,17,19,20]) [1..]
	where allDivis xs x = all (\a -> x `mod` a == 0) xs