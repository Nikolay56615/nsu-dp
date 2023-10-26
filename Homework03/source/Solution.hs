module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs)
    | x `elem` xs = False
    | otherwise = unique xs


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a * a + b * b == c * c]

primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | 
                               (a, b, c) <- pythagoreanTriples, 
                               gcd (gcd a b) c == 1]
    where
      pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a * a + b * b == c * c]

-- >>> take 10 primitivePythagoreanTriples
-- [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29),(12,35,37),(9,40,41),(28,45,53),(11,60,61),(33,56,65)]

perfectNumbers :: Integral a => [a]
perfectNumbers = filter isPerfect [1..]
                  where 
                    isPerfect n = n == sum (divisors n)
                    divisors n = [i | i <- [1..n-1], n `mod` i == 0]

cantorPairs :: Integral a => [(a, a)]
cantorPairs = [(x - y, y) | x <- [0..], y <- [0..x]]

-- >>> take 10 cantorPairs
-- [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3)]

minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
