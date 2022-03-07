module Booking
    (
        book
    ) where 

import System.IO
import Data.Time
import Data.Maybe
import Data.Fixed
import Data.List
import Control.Lens


oneM = 60
oneH = oneM * 60

type Tables = [Table]

data Table = Table {time :: UTCTime,
                       isFree :: Bool,
                       name :: String,
                       phone :: String,
                       persons :: Int 
} deriving Show

data DayL = DayL {date :: UTCTime,
                tables :: Tables,
                openT :: UTCTime,
                closeT :: UTCTime,
                interval :: Int,
                tableNum :: Int
} deriving Show


mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

initDay :: UTCTime ->
           UTCTime ->
           UTCTime ->
           Int ->
           Int ->
           DayL
initDay currDay openT closeT interval numT = 
    DayL currDay timeSlots openT closeT interval numT where
        timeSlots = initTimes openT closeT [] interval numT


initTimes :: UTCTime -> 
             UTCTime -> 
             Tables ->
             Int -> 
             Int -> 
             [Table]    
initTimes openT closeT times interval numT = 
    if diffUTCTime closeT openT >= realToFrac interval then
        time ++ initTimes newOpenT closeT times interval numT 
    else []
    where
        time = initTables numT openT
        newOpenT = addUTCTime (realToFrac interval) openT


initTables :: Int -> UTCTime -> [Table]
initTables 0 _ = [] 
initTables n time = table : initTables (n-1) time where
    table = Table time True "" "" 0


showTimes :: Tables -> [UTCTime]
showTimes times = nub res
    where
        fr = filter(\table -> isFree table) times
        res = map (\x -> time x) fr 


bookTable :: Tables -> 
             UTCTime ->
             String ->
             String ->
             Int ->
             Tables 
bookTable tables bookT name phone persons  = 
    newTables where
        reserveSlots = filter(\x -> bookT <= time x && 
                             (addUTCTime 7200 bookT) > time x) tables 
        times = nub (map (\x -> time x) reserveSlots)
        indexes = map (\time_ -> (findIndices(\table -> 
                                 time table == time_) tables) !! 0) times 
        newTables = helper tables indexes name phone persons


helper :: Tables ->
          [Int] -> 
          String ->
          String ->
          Int ->  
          Tables
helper tables (x:xs) name_ phone_ persons_ =
    if length (x:xs) == 1
        then newTables 
        else helper newTables xs name_ phone_ persons_
    where
        xthTable = tables !! x
        newTables = tables & element x .~ xthTable {isFree = False, 
                                                    name = name_, 
                                                    phone = phone_, 
                                                    persons = persons_}

book :: IO()
book = do  
    currTime <- getCurrentTime
    let currDay = toGregorian $ utctDay currTime 
    let openT = mkUTCTime currDay (10, 0, 0)
    let closeT = mkUTCTime currDay (22, 0, 0)
    -- let dayS = initTimes openT closeT [] 900 5
    -- let newDay = initDay currTime openT closeT (15*oneM) 5 
    -- let dayS = tables newDay
 --   let kek = showTimes dayS
    let bookT = mkUTCTime currDay (10, 30, 0)
    let dayS = initTimes openT closeT [] 3600 5 
    let res = bookTable dayS (mkUTCTime currDay (11, 00, 0)) "Ivan" "777" 2
    putStrLn . show $ res


