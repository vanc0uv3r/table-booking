{-# LANGUAGE OverloadedStrings #-}

module Service where

import Data.Time
import Data.Maybe()
import Data.Fixed
import Data.List
import Control.Lens
import Data.Char


import DataType
import Consts
import Config

-- Get date in format yy-mm-dd hh-mm-ss and return UTCTime object
mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, mins, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour mins sec))


-- get open time, close time, list of tables, interval, number of tables,
-- number of days and returns list of initialized tables
initDays :: OpenTime ->
            CloseTime ->
            Tables ->
            Interval ->
            TablesNum ->
            DaysNum ->
            Tables
initDays openT closeT times intervl numT days =
    if days > 0 then
        day ++ initDays newOpenT newCloseT times intervl numT (days-1)
    else []
    where
        day = initTimes openT closeT [] intervl numT
        newOpenT = addUTCTime (realToFrac oneD) openT
        newCloseT = addUTCTime (realToFrac oneD) closeT


-- Gets open time, close time, list of tables, interval,
-- number of tables and returns list of initialized tables
initTimes :: OpenTime ->
             CloseTime ->
             Tables ->
             Interval ->
             TablesNum ->
             Tables
initTimes openT closeT times intervl numT =
    if diffUTCTime closeT openT >= realToFrac intervl then
        nowTime ++ initTimes newOpenT closeT times intervl numT
    else []
    where
        nowTime = initTables numT openT
        newOpenT = addUTCTime (realToFrac intervl) openT


-- Gets number of table and table time and returns list of
-- equal structures(Table)
initTables :: TablesNum -> UTCTime -> [Table]
initTables 0 _ = []
initTables n currTime = table : initTables (n-1) currTime where
    table = Table currTime True Nothing Nothing Nothing



--Returns Last element of non empty Table list
getLastDay :: Tables -> (Integer, Int, Int)
getLastDay tables = toGregorian $ utctDay lastTime where
                    lastTime = addUTCTime (realToFrac oneD)
                                          (time (last tables))


--Returns List of free tables of choosen day
showDayTables :: Tables ->
                 Day ->
                 Times
showDayTables tables day = res2
    where
        res = filter(\tab -> day == (utctDay $ (time tab))
                                    && isFree tab) tables
        res2 = nub (map (\tab -> time $ tab) res)


--Returns list of strings of times
timeToString :: Times -> [String]
timeToString times = map (formatTime defaultTimeLocale "%H:%M") times


--Returns List of free days
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
bookTable tables bookT intervl currName currPhone currPersons  =
    newTables where
        reserveSlots = filter(\x -> bookT <= time x
                && isFree x
                && (addUTCTime (realToFrac intervl) bookT) > time x) tables
        times = nub (map (\x -> time x) reserveSlots)
        indexes = map (\time_ -> (findIndices(\table ->
                                 time table == time_ &&
                                 isFree table) tables) !! 0) times
        newTables = helper tables indexes False currName
                            currPhone currPersons



-- Gets list of tables, time of booked table, phone
-- returns updated list with unbooked table
unBookTable :: Tables ->
               Phone ->
               Tables
unBookTable tables phone_ =
    newTables where
         indexes = findIndices(\table ->
                              phone table == phone_) tables
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
helper _ _ _ _ _ _ = []


-- Makes numbered list started with n
numberList :: Show a => Int -> [a] -> String
numberList _ [] = ""
numberList num (x:xs) = "\n" ++ show num ++ ". " ++ show x ++
                        numberList (num+1) xs


-- Render list of days
dayWidget :: [Day] -> String
dayWidget tables = chDayMsg ++ res ++ lastPicksMsg
    where res = numberList 1 tables


-- Render list of Times
timesWidget :: Times -> String
timesWidget tables = chTimeMsg ++ res ++ lastPicksMsg
    where res = numberList 1 (timeToString tables)


-- Checks if string is numeric only
isNum :: String -> Bool
isNum ""  = False
isNum "." = False
isNum xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False



-- Check if current phone in database
phoneExists :: Tables -> Phone -> Bool
phoneExists tables ph = case (find (\tab -> phone tab == ph) tables) of
                     Just _ -> True
                     Nothing -> False



showBookedList :: Int -> Tables -> String
showBookedList _ [] = ""
showBookedList num (x:xs) = "\n" ++ show num ++ ". " ++ show (time x)
                    ++ " " ++ n ++ " " ++ p ++
                        showBookedList (num+1) xs where
                        n = case (name x) of
                            Just a -> a
                            _ -> ""
                        p = case (phone x) of
                            Just a -> a
                            _ -> ""


-- Checks admin password
loginAdmin :: String -> Bool
loginAdmin our_pass = our_pass == password

