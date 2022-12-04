#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . find [0] . cycle . map (read . (\(a:as) -> if a == '+' then as else a:as)) . lines

find :: [Int] -> [Int] -> Int
find as [] = error "Reached base case"
find (a:as) (b:bs) = let next = b + a in if elem next (a:as) then next else find (next:a:as) bs