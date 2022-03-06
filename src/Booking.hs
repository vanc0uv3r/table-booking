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

type Tables = [Table]

data TableState = TableState {time :: UTCTime,
                              isFree :: Bool,
                              name :: String,
                              phone :: String,
                              persons :: Int 
} deriving Show

data DayL = DayL {date :: UTCTime,
                tables :: [[Table]],
                openT :: UTCTime,
                closeT :: UTCTime,
                interval :: Int,
                tableNum :: Int
} deriving Show


data Table = Table {id :: Int, 
                    times :: [TableState]
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


initTable :: UTCTime -> 
             UTCTime -> 
             [Table] ->
             Int -> 
             Int -> 
             [Table]    
initTable _ _ _ _ 0 = []
initTable openT closeT tables interval numT = 
    table : initTable openT closeT tables interval (numT-1)
    where 
        table = Table numT times 
        times = initTimes openT closeT [] interval 
      

initTimes :: UTCTime ->
             UTCTime ->
             [TableState] ->
             Int ->
             [TableState]
initTimes openT closeT tables interval = 
    if diffUTCTime closeT openT >= realToFrac interval then
        table : initTimes newOpenT closeT tables interval 
    else []
    where 
        newOpenT = addUTCTime (realToFrac interval) openT
        table = TableState openT False "" "" 0 

book :: IO()
book = do  
    currTime <- getCurrentTime
    let currDay = toGregorian $ utctDay currTime 
    let openT = mkUTCTime currDay (10, 0, 0)
    let closeT = mkUTCTime currDay (22, 0, 0)
    let times = initTimes openT closeT [] (oneM*15) 
    let tables = initTable openT closeT [] (oneM*15) 5
    let newDay = initDay currTime openT closeT 15 5 
    putStrLn . show $ tables 





