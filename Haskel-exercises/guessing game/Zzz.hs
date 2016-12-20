module Zzz where
import System.Random
import System.IO 

check :: String -> String -> Char -> (Bool, String)
check word display c = (c `elem` word, [if x==c then c else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n
     | n==0 = putStrLn ("You lose :( Correct answer: " ++ word) 
     | word==display = putStrLn ("You win :) Correct answer: " ++ word)
     | otherwise = mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n

rollStars :: IO Int
rollStars = getStdRandom (randomR (3,5))

pickLanguage :: String -> IO String
pickLanguage language 
        | language=="PL" = rollWordPl
        | otherwise      = rollWordEng

rollWordPl :: IO String
rollWordPl = do
              n <- getStdRandom (randomR (0,11))
              handle <- openFile "hasla.txt" ReadMode
              contents <- hGetContents handle
              let passwords = words contents
              --hClose handle
              return (passwords !! n)

rollWordEng :: IO String
rollWordEng = do
              n <- getStdRandom (randomR (0,11))
              handle <- openFile "passwords.txt" ReadMode
              contents <- hGetContents handle
              let passwords = words contents
              --hClose handle
              return (passwords !! n)

-- jak po raz kolejny zgaduje tą samą litere, to nie odejmie zycia
-- wezmie 1 znak z wczytanej linii, czyli podanie fakultet to to samo co podanie f