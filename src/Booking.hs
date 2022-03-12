module Booking
    (
        book
    ) where 

import System.IO
import Data.Time
import Data.Maybe
import Data.Fixed
import Data.List
import Debug.Trace
import Control.Lens
import System.Console.ANSI



oneM = 60
oneH = oneM * 60

type Tables = [Table]
type Times = [UTCTime]
type Name = Maybe String
type Phone = Maybe String
type Persons = Maybe Int 
type Interval = Int
type Indexes = [Int]
type TablesNum = Int
type DaysNum = Int
type OpenTime = UTCTime
type CloseTime = UTCTime

data Table = Table {time :: UTCTime,
                    isFree :: Bool,
                    name :: Maybe String,
                    phone :: Maybe String,
                    persons :: Maybe Int 
} deriving Show



-- Get date in format yy-mm-dd hh-mm-ss and return UTCTime object 
mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))


-- get open time, close time, list of tables, interval, number of tables, 
-- number of days and returns list of initialized tables
initDays :: OpenTime -> 
            CloseTime -> 
            Tables ->
            Interval -> 
            TablesNum -> 
            DaysNum ->
            Tables 
initDays openT closeT times interval numT days =
    if days > 0 then  
        day ++ initDays newOpenT newCloseT times interval numT (days-1)
    else []
    where
        day = initTimes openT closeT [] interval numT
        newOpenT = addUTCTime (realToFrac (oneH * 24)) openT
        newCloseT = addUTCTime (realToFrac (oneH * 24)) closeT 


-- Gets open time, close time, list of tables, interval, 
-- number of tables and returns list of initialized tables
initTimes :: OpenTime -> 
             CloseTime -> 
             Tables ->
             Interval -> 
             TablesNum -> 
             Tables 
initTimes openT closeT times interval numT = 
    if diffUTCTime closeT openT >= realToFrac interval then
        time ++ initTimes newOpenT closeT times interval numT 
    else []
    where
        time = initTables numT openT
        newOpenT = addUTCTime (realToFrac interval) openT


-- Gets number of table and table time and returns list of 
-- equal structures(Table) 
initTables :: TablesNum -> UTCTime -> [Table]
initTables 0 _ = [] 
initTables n time = table : initTables (n-1) time where
    table = Table time True Nothing Nothing Nothing 


--Gets list of tables and returns list of UTCTime where isFree==True
showFreeTimes :: Tables -> Times 
showFreeTimes times = nub res
    where
        res = map (time) . filter(isFree) $ times


--Gets list of tables and returns list of UTCTime where isFree==False
showBookTimes :: Tables -> Times 
showBookTimes times = nub res
    where
        res = map (\x -> time x) . filter(not . isFree) $ times


showDayTables :: Tables ->
                 Day ->
                 Times 
showDayTables tables day = res2
    where
        res = filter(\tab -> day == (utctDay $ (time tab))) tables 
        res2 = nub (map (\tab -> time $ tab) res)


timeToString :: Times -> [String]
timeToString times = map (formatTime defaultTimeLocale "%H:%M") times 


showDays :: Tables ->
            [Day] 
showDays tables = res
    where
        tmp = filter (isFree) tables
        res = nub (map (\tab -> utctDay . time $ tab) tmp)


-- Gets list of tables, time for booking, duration of booking, 
-- name, phone, num of persons and return updated list with booked table
bookTable :: Tables -> 
             OpenTime ->
             Interval -> 
             Name ->
             Phone ->
             Persons ->
             Tables 
bookTable tables bookT interval name phone persons  = 
    newTables where
        reserveSlots = filter(\x -> bookT <= time x && 
                   (addUTCTime (realToFrac interval) bookT) > time x) tables
        times = nub (map (\x -> time x) reserveSlots)
        indexes = map (\time_ -> (findIndices(\table -> 
                                 time table == time_ &&
                                 isFree table) tables) !! 0) times 
        newTables = helper tables indexes False name phone persons



-- Gets list of tables, time of booked table, phone
-- returns updated list with unbooked table
unBookTable :: Tables ->
               Interval -> 
               OpenTime -> 
               Phone -> 
               Tables
unBookTable tables interval bookT phone_ = 
    newTables where
         reserveSlots = filter(\x -> bookT <= time x &&
                   (addUTCTime (realToFrac interval) bookT) > time x) tables
         times = nub (map (\x -> time x) reserveSlots)
         indexes = map (\time_ -> (findIndices(\table ->
                                  time table == time_ &&
                                  not (isFree table) &&
                                  phone table == phone_) tables) !! 0) times
         newTables = helper tables indexes True Nothing Nothing Nothing 


-- help function that gets list of tables, indexes of tables to 
-- book/unbook, action(book/unbook), name, phone, persons and 
-- returns updated list of tables with booked/unbooked table
helper :: Tables ->
          Indexes -> 
          Bool ->
          Name ->
          Phone ->
          Persons ->  
          Tables
helper tables (x:xs) book_ name_ phone_ persons_ =
    if length (x:xs) == 1
        then newTables 
        else helper newTables xs book_ name_ phone_ persons_
    where
        xthTable = tables !! x
        newTables = tables & element x .~ xthTable {isFree = book_, 
                                                    name = name_, 
                                                    phone = phone_, 
                                                    persons = persons_}

numberList :: Show a => Int -> [a] -> String
numberList _ [] = ""
numberList num (x:xs) = "\n" ++ show num ++ ". " ++ show x ++ 
                        numberList (num+1) xs


dayWidget :: [Day] -> String
dayWidget tables = tmp ++ res ++ "\n[b]. Go back"
    where res = numberList 1 tables 
          tmp = "Choose the day" 


timesWidget :: Times -> String
timesWidget tables = tmp ++ res ++ "\n[b]. Go back"
    where res = numberList 1 (timeToString tables) 
          tmp = "Choose the time book" 


runner :: Tables -> IO ()
runner tables = do
    let days = showDays tables 
    putStrLn . dayWidget $ days 
    choice <- getLine
    let day = days !! ((read choice) - 1)
    let dayTables = showDayTables tables day
    clearScreen
    putStrLn . timesWidget $ dayTables
    choice <- getLine
    let time = dayTables !! ((read choice) - 1)        
    
    putStrLn . show $ time 


book :: IO()
book = do
    hSetBuffering stdout NoBuffering 
    clearScreen
    currTime <- getCurrentTime
    let currDay = toGregorian $ utctDay currTime 
        openT = mkUTCTime currDay (10, 0, 0)
        closeT = mkUTCTime currDay (22, 0, 0)
        interval = 7200
        tableNum = 2
        days = 3
        name = Just "Ivan"
        phone = Just "777"
        phone2 = Just "888"
        persons = Just 2
        bookT = mkUTCTime currDay (10, 00, 0)
        day = initDays openT closeT [] interval tableNum days 
        --res = bookTable day bookT interval name phone persons
        --res2 = bookTable res bookT interval name phone2 persons 
        --res3 = showDays day
        --res4 = showDayTables day currTime  
    runner day    
    -- putStrLn (numberList 1 res3)
    -- putStrLn (numberList 1 (timeToString res4))
    --putStrLn . show $ showFreeTimes day
    --putStrLn . show $ showFreeTimes res2 
    --putStrLn . show $ res2 
    --putStrLn . show $ showFreeTimes res3 
    --putStrLn . show $ res3 

