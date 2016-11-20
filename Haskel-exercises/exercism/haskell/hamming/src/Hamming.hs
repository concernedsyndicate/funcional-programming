module Hamming (distance) where

distance3 :: Int -> Maybe Int
distance3 x
    | x==(-1) = Nothing
    | otherwise = Just x

distance :: String -> String -> Maybe Int
distance s1 s2 = distance3(distance2 s1 s2)

distance2 :: String -> String -> Int
distance2 [] [] = 0
distance2 [] _ = -1
distance2 _ [] = -1
distance2 (x:xs) (y:ys)
    | length(x:xs)/=length(y:ys) = -1 --nothing po konwersji 
    | x/=y = 1 + distance2 xs ys
    | otherwise = distance2 xs ys
