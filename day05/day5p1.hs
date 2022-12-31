#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.Char
main :: IO ()
main = interact $ show . length . converge reduce . head . lines

converge :: (String -> String) -> String -> String
converge f a = let a' = f a in if a == a' then a else converge f a'

reduce :: String -> String
reduce [] = []
reduce [a] = [a]
reduce (a:b:cs) = if sameLowUp a b then reduce cs else a:(reduce (b:cs))

sameLowUp :: Char -> Char -> Bool
sameLowUp a b = case isLower a of
    True  -> toUpper a == b
    False -> toLower a == b