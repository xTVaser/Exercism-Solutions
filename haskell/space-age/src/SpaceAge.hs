module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthDays :: Planet -> Float
earthDays Mercury = 0.2408467
earthDays Venus = 0.61519726
earthDays Earth = 1.0
earthDays Mars = 1.8808158
earthDays Jupiter = 11.862615
earthDays Saturn = 29.447498
earthDays Uranus = 84.016846
earthDays Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (31557599 * (earthDays planet))
