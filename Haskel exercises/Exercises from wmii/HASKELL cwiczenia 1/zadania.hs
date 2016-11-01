min2 :: Int -> Int -> Int
min2 x y = 
        if x<y then x else y

min3a :: Int -> Int -> Int -> Int
--min3a x y z =
--        if (x<=y && x<=z) then x else
--        if (z<=y && z<=x) then z else y
min3a x y z
        | (x<=y && x<=z) = x
        | (z<=y && z<=x) = z
        | (y<=x && y<=z) = y

min3c :: Int -> Int -> Int -> Int
min3c x y z =
        min2 x y `min2` z

sprawdz :: Int -> String
sprawdz x 
        | x<0 = "liczba mniejsza od 0"
        | (0<=x && x<=10) = "liczba z przedzialu od 0 do 10"
        | x>10 = "liczba wieksza od 10"

albo_albo :: Bool -> Bool -> Bool
albo_albo True True = False
albo_albo True False = True
albo_albo False True = True
albo_albo False False = False

albo_albo2 :: Bool -> Bool -> Bool
albo_albo2 x y = 
            if x then not y else y

pierwiastki :: Float -> Float -> Float -> String
pierwiastki a b c 
            | delta<0 =  "Brak pierwiastkow rzeczywistych"
            | delta==0 = "Jeden pierwiastek rzeczywisty" 
            | delta>0 = " Dwa pierwiastki rzeczywiste"

            where delta = (b^2) - (4*a*c)

pierwiastki2 :: Float -> Float -> Float -> [Float]
pierwiastki2 a b c 
            | delta<0 =  []
            | delta==0 = [(-b)/(2*a)] 
            | delta>0 = [((-b)-sqrt(delta))/(2*a), ((-b)+sqrt(delta))/(2*a)]

            where delta = (b^2) - (4*a*c)