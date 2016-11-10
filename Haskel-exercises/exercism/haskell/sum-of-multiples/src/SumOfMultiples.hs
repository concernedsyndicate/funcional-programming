module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples [] _ = 0
sumOfMultiples (x:xs) n = sum (removeDuplicates (addMultiples (x:xs) n))

addMultiples :: [Int] -> Int -> [Int] -- bierze liste startowa, a zwraca liste wielokrotnosci x'ow < n, z duplikatami
addMultiples [] _ = []
addMultiples (x:xs) n 
    | x<n = [ a | a <- map (*x) [1,2..n], a<n] ++ addMultiples xs n 
    | otherwise = addMultiples xs n

removeDuplicates :: [Int] -> [Int] -- usuwa duplikaty
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs