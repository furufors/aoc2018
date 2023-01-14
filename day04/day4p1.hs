#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Text.Parsec
import qualified Data.Map as M
import Data.List
import Data.Time
import Data.Ord
type NightShift = [Bool] -- 00:00--00:59 True <-> Awake, False <-> Asleep
data Event = ShiftStart Guard | FallAsleep | WakeUp deriving (Eq, Show)
type TimeStamp = (Int, Int, Int, Int, Int)
type Guard = Int
type TimedEvent = (UTCTime, Event)
type TimeSeries = [TimedEvent]
type GuardStatistics = M.Map Guard [(UTCTime,NightShift)]

main :: IO ()
main = interact $ show . strategy1 . buildStatistics . groupByShift . map parsein . lines

strategy1 :: GuardStatistics -> Int
strategy1 gs = guardId * (bestMinute guardId)
    where
        (guardId, _) = head . reverse . sortBy (comparing snd) . map countSleepTime . M.assocs $ gs
        countSleepTime :: (Guard,[(UTCTime,NightShift)]) -> (Guard, Int)
        countSleepTime (g,tns) = (g, sum $ map (length . filter (==False) . snd) tns)
        bestMinute gid =
            let lst = case M.lookup gid gs of
                    Just tns -> map snd tns
                    Nothing  -> []
                sleepCounts = zip [0..59] $ foldl' sleepCount (take 60 $ repeat 0) $ lst
                (bm,_) = head . reverse $ sortBy (comparing snd) sleepCounts
            in bm
        sleepCount :: [Int] -> [Bool] -> [Int]
        sleepCount is bs = map incIfFalse $ zip is bs
        incIfFalse (i, True) = i
        incIfFalse (i, False) = i + 1

groupByShift :: TimeSeries -> [TimeSeries]
groupByShift = groupBy (\(t,e) (t1,e1) -> (alignShift t) == (alignShift t1))
    . sortBy (\(t,e) (t1,e1) -> compare t t1)

buildStatistics :: [TimeSeries] -> GuardStatistics
buildStatistics tss = foldl' addStatistic M.empty tss'
    where
        tss' :: [(Int,(UTCTime, NightShift))]
        tss' = map toGuardNightShift tss
        toGuardNightShift :: TimeSeries -> (Int, (UTCTime, NightShift))
        toGuardNightShift ts = (findGuard ts, toNightShift ts)
        findGuard :: TimeSeries -> Int
        findGuard [] = error "No guard at shift!"
        findGuard ((_,(ShiftStart g)):ts) = g
        findGuard ((_,WakeUp):ts) = findGuard ts
        findGuard ((_,FallAsleep):ts) = findGuard ts
        toNightShift :: TimeSeries -> (UTCTime, NightShift)
        toNightShift ts =
            let shift0 = alignShift . fst . head $ ts
                ns = map (checkStatus True ts) [addUTCTime (fromIntegral $ x * 60) shift0 | x <- [0..59]]
            in (shift0, ns)
        checkStatus :: Bool -> TimeSeries -> UTCTime -> Bool
        checkStatus b [] t = b
        checkStatus b ((et,(ShiftStart g)):ts) t = checkStatus b ts t
        checkStatus b ((et,(FallAsleep)):ts) t = if et <= t
                                                 then checkStatus False ts t
                                                 else checkStatus b ts t
        checkStatus b ((et,(WakeUp)):ts) t = if et <= t
                                             then checkStatus True ts t
                                             else checkStatus b ts t
        addStatistic :: GuardStatistics -> (Int,(UTCTime, NightShift)) -> GuardStatistics
        addStatistic gs (g,ns) = case M.lookup g gs of
            Just lst -> M.insert g (ns:lst) gs
            Nothing  -> M.insert g (ns:[ ]) gs

alignShift :: UTCTime -> UTCTime
alignShift t =
    let tod = localTimeOfDay $ utcToLocalTime utc t
    in case todHour tod  of
        23 -> addUTCTime (fromIntegral $ (60-(todMin tod))*60) t
        00 -> addUTCTime (fromIntegral $ ( 0-(todMin tod))*60) t
        __ -> t

parsein :: String -> TimedEvent
parsein input = case parse event "parsein" input of
    Left err -> error $ show err
    Right a -> a

event :: Parsec String () TimedEvent
event = do
    t <- parseTimestamp
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

parseTimestamp :: Parsec String () UTCTime
parseTimestamp = do
    _  <- string "["
    y  <- many1 digit
    _  <- string "-"
    m  <- many1 digit
    _  <- string "-"
    d  <- many1 digit
    _  <- string " "
    hh <- many1 digit
    _  <- string ":"
    mm <- many1 digit
    _  <- string "] "
    let utc = parseTimeOrError True defaultTimeLocale "%Y,%m,%d,%H,%M" . intercalate "," $ [y,m,d,hh,mm]
    return utc