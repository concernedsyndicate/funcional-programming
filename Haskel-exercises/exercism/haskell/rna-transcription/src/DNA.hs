module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA (x:xs)
    | null (toRNA' (x:xs)) = Nothing
    | otherwise = Just (toRNA' (x:xs))

toRNA' :: String -> String
toRNA' [] = []
toRNA' (x:xs)
    | not (isOK (x:xs)) = []
    | x == 'G' = "C" ++ toRNA' xs
    | x == 'C' = "G" ++ toRNA' xs
    | x == 'T' = "A" ++ toRNA' xs
    | x == 'A' = "U" ++ toRNA' xs
    | otherwise = []

isOK :: String -> Bool
isOK [] = True
isOK (x:xs) = (x=='G' || x=='C' || x=='T' || x=='A') && isOK xs