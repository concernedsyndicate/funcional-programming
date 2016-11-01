module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
    | planet==Earth = seconds/earthYear
    | planet==Mercury = seconds/(earthYear * 0.2408467)
    | planet==Venus = seconds/(earthYear * 0.61519726)
    | planet==Mars = seconds/(earthYear * 1.8808158)
    | planet==Jupiter = seconds/(earthYear * 11.862615)
    | planet==Saturn = seconds/(earthYear * 29.447498)
    | planet==Uranus = seconds/(earthYear * 84.016846)
    | planet==Neptune = seconds/(earthYear * 164.79132)
    | otherwise = 0
    where earthYear = 31557600