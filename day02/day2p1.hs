#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
main :: IO ()
main = interact $ show . toResult . map (\line -> (hasTwo line, hasThree line)) . lines

toResult :: [(Bool,Bool)] -> Int
toResult bs = let numTwo = length . filter id . map fst $ bs
                  numThree = length . filter id . map snd $ bs
              in numTwo * numThree

hasNum :: Int -> String -> Bool
hasNum i s = i `elem` (map length . group . sort $ s)

hasTwo :: String -> Bool
hasTwo = hasNum 2

hasThree :: String -> Bool
hasThree = hasNum 3