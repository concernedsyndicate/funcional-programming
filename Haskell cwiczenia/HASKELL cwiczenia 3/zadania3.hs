sigma :: Float -> Float -> (Float -> Float) -> Float
sigma a b f =
        if a>b then 0 else (f a) + sigma (a+1) b f

sigma1 :: Float -> Float -> Float
sigma1 a b = sigma a b (\x -> x*x - x)

sigma2 :: Float -> Float
sigma2 n = sigma 0 (n-1) (\x -> (1/((1+4*x)*(3+4*x))))

pii = 8*(sigma2 1000)

calk_ozn :: Float -> Float -> (Float -> Float) -> Float -> Float
calk_ozn a b f n =
        let dx = (b-a)/n
        in dx*(sum ((map f) [a+dx, a+2*dx..b]))