#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import qualified Data.Map as M
type Pos = (Int,Int)
type Square = (Pos, Int, Int, Int) -- (x,y) w h i

main :: IO ()
main = interact $ show . (\x -> findNonOverlapping x (foldl' collect M.empty x)) . map parsein . lines

findNonOverlapping :: [Square] -> M.Map Pos Int -> Int
findNonOverlapping [] _ = error "All overlap."
findNonOverlapping (((x,y),w,h,i):ss) m =
    if all (==(Just 1)) [M.lookup (a,b) m | dx <- [0..(w-1)], dy <- [0..(h-1)], let a = x+dx, let b = y+dy]
    then i
    else findNonOverlapping ss m

collect :: M.Map Pos Int -> Square -> M.Map Pos Int
collect m ((x,y),w,h,i) = foldl' inserter m [(a,b) | dx <- [0..(w-1)], dy <- [0..(h-1)], let a = x+dx, let b = y+dy]
    where
        inserter :: M.Map Pos Int -> Pos -> M.Map Pos Int
        inserter m p = case M.lookup p m of
            Just a  -> M.insert p (a+1) m
            Nothing -> M.insert p 1 m

parsein :: String -> Square
parsein input = case parse square "parsein" input of
    Left err -> error $ show err
    Right a -> a

square :: Parsec String () Square
square = do
    _  <- string "#"
    i  <- read <$> many1 digit
    _  <- string " @ "
    x  <- read <$> many1 digit
    _  <- string ","
    y  <- read <$> many1 digit
    _  <- string ": "
    w  <- read <$> many1 digit
    _  <- string "x"
    h  <- read <$> many1 digit
    return $ ((x,y),w,h,i)