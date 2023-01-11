#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import qualified Data.Map as M
import Data.List
import Data.Time
type NightShift = [Bool] -- 00:00--00:59 True <-> Awake, False <-> Asleep
data Event = ShiftStart Guard | FallAsleep | WakeUp deriving (Eq, Show)
type TimeStamp = (Int, Int, Int, Int, Int)
type Guard = Int
type TimedEvent = (TimeStamp, Event)
type TimeSeries = [TimedEvent]
type GuardStatistics = M.Map Guard [NightShift]

main :: IO ()
main = interact $ intercalate "\n" . map show . toShift . sortBy cmpMinutes . map parsein . lines

toShift :: TimeSeries -> [(Int, TimeSeries)]
toShift [] = []
toShift (():rest)

cmpMinutes :: TimedEvent -> TimedEvent -> Ordering
cmpMinutes (t1,_) (t2,_) = compare (toMinutes t1) (toMinutes t2)

toMinutes :: TimeStamp -> Int
toMinutes (y,m,d,hh,mm) = (((y * 12 + m) * 31 + d) * 24 + hh) * 60 + mm

toShift :: TimeStamp -> String
toShift (y,m,d,)

parsein :: String -> TimedEvent
parsein input = case parse event "parsein" input of
    Left err -> error $ show err
    Right a -> a

event :: Parsec String () TimedEvent
event = do
    t <- parseTime
    e <- try shiftStart <|> try fallAsleep <|> try wakeup
    return (t,e)

shiftStart :: Parsec String () Event
shiftStart = do
    _ <- string "Guard #"
    i <- read <$> many1 digit
    _ <- string " begins shift"
    return $ ShiftStart i

fallAsleep :: Parsec String () Event
fallAsleep = string "falls asleep" >> return FallAsleep

wakeup :: Parsec String () Event
wakeup = string "wakes up" >> return WakeUp

parseTime :: Parsec String () TimeStamp
parseTime = do
    _  <- string "["
    y  <- read <$> many1 digit
    _  <- string "-"
    m  <- read <$> many1 digit
    _  <- string "-"
    d  <- read <$> many1 digit
    _  <- string " "
    hh <- read <$> many1 digit
    _  <- string ":"
    mm <- read <$> many1 digit
    _  <- string "] "
    return $ (y,m,d,hh,mm)