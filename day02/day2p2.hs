#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ findCombinations . lines

findCombinations :: [String] -> String
findCombinations ss = head [takeCommon s1 s2 | s1 <- ss, s2 <- ss, s1 /= s2, exactlyOneDiff s1 s2 ]

takeCommon :: String -> String -> String
takeCommon [] _ = []
takeCommon _ [] = []
takeCommon (a:as) (b:bs) = if a == b then a:(takeCommon as bs) else takeCommon as bs

exactlyOneDiff :: String -> String -> Bool
exactlyOneDiff s1 s2 =
    let pairs = zip s1 s2
    in (1==) . length . filter (not) . map (\(x,y) -> x == y) $ pairs