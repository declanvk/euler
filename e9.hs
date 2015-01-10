--Project Euler Problem 9 -- Special Pythagorean triplet

answer :: Int
answer = head $ map (\(a,b,c) -> a*b*c) . take 1 $ filter (\(a,b,c) -> a + b + c == 1000 ) pythagTriples

pythagTriples :: [(Int, Int, Int)]
pythagTriples = [ (a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a < b && b < c]