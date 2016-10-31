iloczyn_listy :: [Int]->Int
iloczyn_listy [a] = a
iloczyn_listy (x:xs) = x * iloczyn_listy xs

kwadrat_listy :: [Int]->[Int]
kwadrat_listy l = [x*x | x <- l]

kwadrat_listy2 :: [Int]->[Int]
kwadrat_listy2 l = map kwadrat l where kwadrat x = x*x

kwadrat_listy3 :: [Int]->[Int]
kwadrat_listy3 l = map (\x -> x*x) l

usun :: Int ->[Int]->[Int]
usun _ [] = []
usun a (x:xs) =
       if a/=x then x:(usun a xs) else usun a xs

ostatni :: [Int] -> Int
ostatni l = head (reverse l)

ostatni2 :: [Int] -> Int
ostatni2 l = l !! ((length l) - 1)

usun_ostatni :: [Int] -> [Int]
usun_ostatni l = reverse(tail(reverse (l)))

dzielniki :: Int -> [Int]
dzielniki x =
        if x<0 then []
        else [a | a<-[1..x], x `mod` a == 0]

pierwsza :: Int -> Bool
pierwsza n =
        if dzielniki n == [1,n] then True else False

pierwsze :: Int -> [Int]
pierwsze n =
        filter pierwsza [1..(n-1)]