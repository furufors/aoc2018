#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import Data.List
import qualified Data.Map as M
type Pos = (Int,Int)
type Square = (Pos, Int, Int) -- (x,y) w h

main :: IO ()
main = interact $ show . counts . foldl' collect M.empty . map parsein . lines

counts :: M.Map Pos Int -> Int
counts m = length . filter (>1) $ M.elems m

collect :: M.Map Pos Int -> Square -> M.Map Pos Int
collect m ((x,y),w,h) = foldl' inserter m [(a,b) | dx <- [0..(w-1)], dy <- [0..(h-1)], let a = x+dx, let b = y+dy]
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
    _  <- many1 digit
    _  <- string " @ "
    x  <- read <$> many1 digit
    _  <- string ","
    y  <- read <$> many1 digit
    _  <- string ": "
    w  <- read <$> many1 digit
    _  <- string "x"
    h  <- read <$> many1 digit
    return $ ((x,y),w,h)