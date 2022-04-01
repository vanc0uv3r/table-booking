{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Booking
    (
        book
    ) where 


import System.Directory
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Time
import Data.Maybe()
import Data.Fixed
import Data.List
import Control.Lens
import System.Console.ANSI
import System.Exit
import Data.Char

import Consts
import Config


data State =
      Role
    | ChooseDay 
    | ChooseTime
    | EditForm


type Tables = [Table]
type Times = [UTCTime]
type Name = Maybe String
type Phone = Maybe String
type LastInput = Maybe String
type Persons = Maybe Int 
type Interval = Integer
type Indexes = [Int]
type TablesNum = Integer
type DaysNum = Integer
type OpenTime = UTCTime
type CloseTime = UTCTime
type ToReturn = Bool


minDays :: Integer
maxDays :: Integer

minDays = 0
maxDays = 100

data Table = Table {time :: UTCTime,
                    isFree :: Bool,
                    name :: Maybe String,
                    phone :: Maybe String,
                    persons :: Maybe Int 
} deriving (Show, Generic)



instance FromJSON Table 
instance ToJSON Table 


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


-- Saves list of tables into file
saveTables :: Tables -> IO()
saveTables tables = do
            let encoded = encode $ tables
            B.writeFile cnfName encoded


-- Render unbook interface (prompt to phone number)
unBookWidget :: Tables -> IO()
unBookWidget tables = do
             clearScreen
             putStrLn $ enterPhoneMsg 
             currPhone <- getLine
             if currPhone == "b" then
                 adminRunner tables
             else if (phoneExists tables (Just currPhone)) then do 
                let newTables = unBookTable tables (Just currPhone)
                saveTables newTables
                adminRunner newTables
             else
                unBookWidget tables


-- Render addDays interface (prompt to days number)
addDaysWidget :: Tables -> IO()
addDaysWidget tables = do
              clearScreen
              putStrLn $ enterDaysMsg
              days <- getLine
              if days == "b" then
                 adminRunner tables
              else if isNum days && (read days) > minDays 
                      && (read days) < maxDays then do
                  let currDay = getLastDay $ tables 
                      openT = mkUTCTime currDay openH
                      closeT = mkUTCTime currDay closeH
                      newTables = initDays openT closeT [] 
                           interval tableNum (read days)
                  saveTables (tables ++ newTables)
                  adminRunner (tables ++ newTables)
              else
                  addDaysWidget tables


-- Render initDays interface (prompt to days number)
initDaysWidget :: Tables -> IO()
initDaysWidget tables = do
               clearScreen
               putStrLn $ enterDaysMsg
               days <- getLine
               if days == "b" then
                   adminRunner tables
               else if isNum days && (read days) > minDays 
                      && (read days) < maxDays then do
                   currTime <- getCurrentTime
                   let currDay = toGregorian $ utctDay currTime 
                       openT = mkUTCTime currDay openH
                       closeT = mkUTCTime currDay closeH
                       newTables = initDays openT closeT [] 
                          interval tableNum (read days)
                   saveTables newTables
                   adminRunner newTables
               else
                   initDaysWidget tables 

showTodayBooking :: Tables -> IO()
showTodayBooking tables = do
            let booked = filter (\tab -> not (isFree tab)) tables
                bookedStr = showBookedList 1 booked
            putStrLn bookedStr 
            


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


-- Handle action in admin panel and then proccess it
adminRunner :: Tables -> IO ()
adminRunner tables = do
            clearScreen
            putStrLn $ adminMsg 
            choice <- getLine
            case choice of
                "1" -> do
                    clearScreen
                    runner tables ChooseDay Nothing Nothing True
                "2" -> unBookWidget tables
                "3" -> initDaysWidget tables
                "4" -> do
                    if (length tables) == 0 then
                        initDaysWidget tables
                    else
                        addDaysWidget tables
                "5" -> do 
                        showTodayBooking tables
                        putStrLn $ contMsg
                        _ <- getLine
                        adminRunner tables
                "q" -> die(quitMsg)
                "l" -> runner tables Role Nothing Nothing False
                _ -> adminRunner tables


-- Render choice of action (log in or book as a usual user)
rWidget :: Tables   -> 
           State    -> 
           LastInput   -> 
           LastInput   -> 
           ToReturn ->
           IO()
rWidget tables _ _ _ ret = do
          putStrLn $ roleMsg 
          choice <- getLine
          if choice == "2" then do
              clearScreen
              putStr $ passMsg
              pass <- getLine
              if (loginAdmin pass) then
                 adminRunner tables
              else do
                 putStrLn $ wrongPassMsg 
                 rWidget tables Role Nothing Nothing False
          else if choice == "1" then 
              runner tables ChooseDay Nothing Nothing ret
          else if choice == "q" then
              die(quitMsg)
          else
              runner tables Role Nothing Nothing ret
  
  
-- Render interface of choosing day to book
chDaysWidget :: Tables   ->
                State    ->
                LastInput   ->
                LastInput   ->
                ToReturn ->
                IO()
chDaysWidget tables _ _ _ ret = do
        let days = showDays tables
            dLen = length days
        clearScreen
        if dLen == 0 then do
            putStrLn $ noDaysMsg
            _ <- getLine
            if ret then 
                adminRunner tables
            else
                runner tables Role Nothing Nothing ret
        else do
             putStrLn . dayWidget $ days 
             choice <- getLine
             if choice == "b" then do
                if ret then
                    adminRunner tables 
                else
                    runner tables Role Nothing Nothing ret
             else if choice == "q" then
                die(quitMsg)
             else if isNum choice && (read choice) <= dLen
                             && (read choice) > minDays then 
                  runner tables ChooseTime (Just choice) Nothing ret
             else do
                putStrLn $ invOptMsh 
                runner tables ChooseDay Nothing Nothing ret


-- Render interface of choosing time to book
chTimeWidget :: Tables   ->
                State    ->
                LastInput   ->
                LastInput   ->
                ToReturn ->
                IO()
chTimeWidget tables _ choice1 _ ret = do
        let days = showDays tables
            day = days !! (ch - 1) where
                ch = case choice1 of
                    Just a -> (read a)
                    Nothing -> 0
            dayTables = showDayTables tables day
            dLen = length dayTables
        if dLen == 0 then do
            putStrLn $ noDaysMsg
            _ <- getLine
            if ret then 
                adminRunner tables
            else
                runner tables Role Nothing Nothing ret
        else do
            clearScreen
            putStrLn . timesWidget $ dayTables
            choice <- getLine
            if choice == "b" then
                runner tables ChooseDay Nothing Nothing ret
            else if choice == "q" then 
                die(quitMsg)
            else if isNum choice && (read choice) <= dLen
                             && (read choice) > minDays then 
                runner tables EditForm choice1 (Just choice) ret
            else do
                putStrLn $ invOptMsh 
                runner tables ChooseTime choice1 Nothing ret


-- Renders form to fill for booking table 
formWidget :: Tables   ->
              State    ->
              LastInput   ->
              LastInput   ->
              ToReturn ->
              IO()
formWidget tables _ choice1 choice2 ret = do
            let days = showDays tables
                day = days !! (ch - 1) where
                    ch = case choice1 of
                        Just a -> (read a)
                        Nothing -> 0
                dayTables = showDayTables tables day
                bookTime = dayTables !! (ch - 1) where
                    ch = case choice2 of
                        Just a -> (read a)
                        Nothing -> 0
            putStrLn $ nameMsg
            currName <- getLine
            putStrLn $ phoneMsg
            currPhone <- getLine 
            putStrLn $ personsMsg 
            currPersons <- getLine
            clearScreen
            if not (isNum currPersons) then do
                putStrLn $ personsErrMsg 
                runner tables EditForm choice1 choice2 ret
            else if not (isNum currPhone) then do
                putStrLn $ phoneErrMsg 
                runner tables EditForm choice1 choice2 ret
            else if (phoneExists tables (Just currPhone)) then do
                putStrLn $ uniquePhoneMsg 
                runner tables EditForm choice1 choice2 ret
            else do
                putStrLn $ checkDataMsg ++ currName ++ "\n" ++ currPhone 
                                        ++ "\n" ++ currPersons
                putStrLn $ correctMsg 
                c <- getLine
                if c /= "y" then
                    runner tables EditForm choice1 choice2 ret
                else do 
                    let days2 = bookTable tables bookTime bookInterval 
                               (Just currName) (Just currPhone) 
                               (Just(read currPersons))
                    saveTables days2 
                    putStr $ successMsg 
                    putStrLn . show $ bookTime
                    putStrLn $ contMsg
                    _ <- getLine
                    if ret then
                        adminRunner days2
                    else
                        runner days2 Role Nothing Nothing ret


-- Handle menu action and then proccess it 
runner :: Tables   ->
          State    -> 
          LastInput -> 
          LastInput ->
          ToReturn ->
          IO ()
runner tables widget choice1 choice2 ret = do
    case widget of
        Role -> do
            clearScreen
            rWidget tables widget Nothing Nothing ret
        ChooseDay -> do
            chDaysWidget tables widget choice1 choice2 ret
        ChooseTime -> do
            chTimeWidget tables widget choice1 choice2 ret
        EditForm -> do 
            formWidget tables widget choice1 choice2 ret


book :: IO()
book = do
    hSetBuffering stdout NoBuffering 
    clearScreen
    fileExist <- doesFileExist cnfName
    if not fileExist then
         writeFile cnfName ""
    else return ()
    contents <- BS.readFile cnfName 
    let loadedTables = decodeStrict contents :: Maybe Tables 
        tables = case loadedTables of
              Just a -> a
              Nothing -> []
    runner tables Role Nothing Nothing False    

