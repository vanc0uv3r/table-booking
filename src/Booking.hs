module Booking
    (
        book
    ) where 

import System.IO
import Data.Time
import Data.Maybe
import Data.Fixed


oneM = 60
oneH = oneM * 60

type Tables = [(UTCTime, [TableState])]

data TableState = TableState {isFree :: Bool,
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
    DayL currDay [] openT closeT interval numT


initTimes :: UTCTime -> 
             UTCTime -> 
             Tables ->
             Int -> 
             Int -> 
             Tables    
initTimes openT closeT times interval numT = 
    if diffUTCTime closeT openT >= realToFrac interval then
        time : initTimes newOpenT closeT times interval numT 
    else []
    where
        time = (openT, tables)
        tables = initTables numT 
        newOpenT = addUTCTime (realToFrac interval) openT

initTables :: Int -> [TableState]
initTables 0 = [] 
initTables n = table : initTables (n-1) where
    table = TableState True "" "" 0 


book :: IO()
book = do  
    currTime <- getCurrentTime
    let currDay = toGregorian $ utctDay currTime 
    let openT = mkUTCTime currDay (10, 0, 0)
    let closeT = mkUTCTime currDay (22, 0, 0)
    let dayS = initTimes openT closeT [] 900 5
    let newDay = initDay currTime openT closeT 15 5 
    let res = snd (dayS !! 1)
    let fi = filter(\x -> isFree x == False) res
    putStrLn . show $ fi 





