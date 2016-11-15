module Grains (square, total) where

square :: Integer -> Maybe Integer
square x 
    | 0<x && x<65 = Just $ 2^(x-1)
    | otherwise = Nothing

total :: Integer
total = 18446744073709551615 --2^64 - 1