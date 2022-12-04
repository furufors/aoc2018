#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . sum . map (read . (\(a:as) -> if a == '+' then as else a:as)) . lines