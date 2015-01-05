--Project Euler Problem 4 -- Largest palindrome product

answer :: Integer
answer = maximum $ [ c | a <- [1..999], b <- [1..999], let c = a * b,  isPalindrome $ digits c]

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome all = (f == l) && (isPalindrome rest)
    where   f = head all
            l = last all
            rest = tail $ init all