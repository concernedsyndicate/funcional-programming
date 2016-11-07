module Bob (responseFor) where

import GHC.Unicode
responseFor :: String -> String
responseFor x
    | allHuge x && gotLetters x = "Whoa, chill out!"
    | isEmptyOrSpace x = "Fine. Be that way!"
    | isQuestion x = "Sure."
    | otherwise = "Whatever."

isEmptyOrSpace :: String -> Bool
isEmptyOrSpace [] = True
isEmptyOrSpace (y:ys) =
 isSpace y && isEmptyOrSpace ys


isQuestion :: String -> Bool
isQuestion [] = False
isQuestion (y:ys)
    | last (y:ys) == '?' = True
    | last (y:ys) == ' ' = isQuestion (take (length (y:ys) - 1) (y:ys))
    | otherwise = False




allHuge :: String -> Bool
allHuge [] = True
allHuge (y:ys) =
    isNotLower y && allHuge ys

isNotLower :: Char -> Bool
isNotLower c = not(isLower c)

gotLetters  :: String -> Bool
gotLetters [] = False
gotLetters (y:ys) =
    isLetter y || gotLetters ys

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c