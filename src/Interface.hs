module Interface where


import DataType
import Consts
import Config
import Service

import System.Exit
import System.Console.ANSI
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Time

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
