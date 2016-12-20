import Zzz

main :: IO()
main = do 
          putStrLn "PL or ENG?"
          language <- getLine
          word <- (pickLanguage language)
          n <- rollStars -- player will have 3-5 hp
          starman word n