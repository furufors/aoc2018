#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
main :: IO ()
main = interact $ show . reduceMaximally . head . lines

reduceMaximally :: String -> Int
reduceMaximally s = minimum [length $ converge reduce s' | l <- ['a'..'z'], let s' = filter (/= l) . filter (/= (toUpper l)) $ s]

converge :: Eq d => (d -> d) -> d -> d
converge f a = let a' = f a in if a == a' then a else converge f a'

reduce :: String -> String
reduce [] = []
reduce [a] = [a]
reduce (a:b:cs) = if sameLowUp a b then reduce cs else a:(reduce (b:cs))

sameLowUp :: Char -> Char -> Bool
sameLowUp a b = case isLower a of
    True  -> toUpper a == b
    False -> toLower a == b