{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Booking
    (
        book
    ) where 

import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Time
import Data.Maybe
import Data.Fixed
import Data.List
import Debug.Trace
import Control.Lens
import System.Console.ANSI
import Debug.Trace
import System.Exit
import Data.Char

data State =
      Role
    | ChooseDay 
    | ChooseTime
    | EditForm

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
} deriving (Show, Generic)



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
        res = filter(\tab -> day == (utctDay $ (time tab))
                                    && isFree tab) tables 
        res2 = nub (map (\tab -> time $ tab) res)


timeToString :: Times -> [String]
timeToString times = map (formatTime defaultTimeLocale "%H:%M") times 


showDays :: Tables ->
            [Day] 
showDays tables = res
    where
        tmp = filter (isFree) tables


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
        reserveSlots = filter(\x -> bookT <= time x 
                 && isFree x 
                 && (addUTCTime (realToFrac interval) bookT) > time x) tables
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
dayWidget tables = tmp ++ res ++ "\n[b].Go back\n[q].Quit"
    where res = numberList 1 tables 
          tmp = "Choose the day" 


timesWidget :: Times -> String
timesWidget tables = tmp ++ res ++ "\n[b]. Go back\n[q].Quit"
    where res = numberList 1 (timeToString tables) 
          tmp = "Choose the time book" 


roleWidget :: String
roleWidget = "Welcome to table booking system\n1.Book a table\n" ++
             "2.Login to admin panel\n[q] Quit"


isNum :: String -> Bool
isNum ""  = False
isNum "." = False
isNum xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False


--adminUnbook :: Tables -> IO()
--adminUnbook tables = do
                        


adminRunner :: Tables -> IO ()
adminRunner tables = do
            clearScreen
            putStrLn "Choose the action:\n1. Book table\n2. Unbook table\n3. Init mounth\n[q] Logout"
            choice <- getLine
            case choice of
                "1" -> do
                    clearScreen
                    runner tables ChooseDay "" ""


rWidget :: Tables -> State -> String -> String -> IO()
rWidget tables widget _ _ = do
          putStrLn $ roleWidget 
          choice <- getLine
          if choice == "2" then
              adminRunner tables
          else if choice == "1" then 
              runner tables ChooseDay "" ""
          else if choice == "q" then
              die("Quiting...")
          else
              runner tables Role "" ""


chDaysWidget :: Tables ->
                State  ->
                String ->
                String ->
                IO()
chDaysWidget tables widget choice1 choice2 = do
        let days = showDays tables 
        clearScreen
        putStrLn . dayWidget $ days 
        choice <- getLine
        if choice == "b" 
            then runner tables Role "" "" 
            else if choice == "q" 
                then die("Quiting...")
                else if isNum choice 
                    then runner tables ChooseTime choice ""
                    else do
                        putStrLn "Invalid option"
                        runner tables ChooseDay "" ""


chTimeWidget :: Tables ->
                State  ->
                String ->
                String ->
                IO()
chTimeWidget tables widget choice1 choice2 = do
        let days = showDays tables
            day = days !! ((read choice1) - 1)
            dayTables = showDayTables tables day
        clearScreen
        putStrLn . timesWidget $ dayTables
        choice <- getLine
        if choice == "b" 
            then runner tables ChooseDay "" "" 
            else if choice == "q" 
                then die("Quiting...")
                else if isNum choice 
                    then runner tables EditForm choice1 choice
                    else do
                        putStrLn "Invalid option"
                        runner tables ChooseTime choice1 ""


formWidget :: Tables ->
              State  ->
              String ->
              String ->
              IO()
formWidget tables widget choice1 choice2 = do
            let days = showDays tables
                day = days !! ((read choice1) - 1)
                dayTables = showDayTables tables day
                bookTime = dayTables !! ((read choice2) - 1)        
            putStrLn "Enter your name:"
            name <- getLine
            putStrLn "Enter your phone:"
            phone <- getLine 
            putStrLn "Enter number of persons:"
            persons <- getLine
            clearScreen
            if not (isNum persons) then do
                putStrLn "Persons should be number" 
                runner tables EditForm choice1 choice2
            else do
                putStrLn ("Check your data:\n" ++ name ++ "\n" ++ 
                          phone ++ "\n" ++ persons) 
                putStrLn "Is it correct?[y]"
                c <- getLine
                if c /= "y" then
                    runner tables EditForm choice1 choice2
                else do 
                    let kek = Just name 
                        kek1 = Just phone
                        kek2 = Just(read persons) 
                        days2 = bookTable tables bookTime 7200 kek kek1 kek2
                    putStr "You have successfuly booked the table on "
                    putStrLn . show $ bookTime
runner :: Tables ->
          State -> 
          String -> 
          String ->
          IO ()
runner tables widget choice1 choice2 = do
    case widget of
        Role -> do
            clearScreen
            rWidget tables widget "" ""
        ChooseDay -> do
            chDaysWidget tables widget choice1 choice2
        ChooseTime -> do
            chTimeWidget tables widget choice1 choice2
        EditForm -> do 
            formWidget tables widget choice1 choice2

readConfig :: IO()
readConfig = do
        putStrLn . show $ contents

book :: IO()
book = do
    hSetBuffering stdout NoBuffering 
    clearScreen
    currTime <- getCurrentTime
    handle <- openFile "cfg.txt" ReadMode
    contents <- hGetContents handle    
    let currDay = toGregorian $ utctDay currTime 
        openT = mkUTCTime currDay (10, 0, 0)
        closeT = mkUTCTime currDay (22, 0, 0)
        interval = 3600 
        tableNum = 1 
        days = 1 
        name = Just "Ivan"
        phone = Just "777"
        phone2 = Just "888"
        persons = Just 2
        bookT = mkUTCTime currDay (10, 00, 0)
        day = initDays openT closeT [] interval tableNum days 
    readConfig 
    -- runner day Role "" ""    

