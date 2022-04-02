module Consts where

oneM = 60
oneH = oneM * 60
oneD = 24 * oneH


minDays :: Integer
maxDays :: Integer

minDays = 0
maxDays = 100

oneM :: Int
oneH :: Int
oneD :: Int 
nameMsg :: String
phoneMsg :: String
personsMsg :: String
correctMsg :: String
personsErrMsg :: String
successMsg :: String
quitMsg :: String
chDayMsg :: String
chTimeMsg :: String
invOptMsh :: String
rWidgetMsg :: String
adminMsg :: String
roleMsg :: String
lastPicksMsg :: String
checkDataMsg :: String
enterPhoneMsg :: String
enterDaysMsg :: String
noDaysMsg :: String
passMsg :: String
wrongPassMsg :: String
phoneErrMsg :: String
uniquePhoneMsg :: String
contMsg :: String

nameMsg = "Enter your name:"

phoneMsg = "Enter your phone:"

personsMsg = "Enter number of persons:"

correctMsg = "Is it correct?[y]"

personsErrMsg = "Persons should be number"

successMsg = "You have successfuly booked the table on "

quitMsg = "Quiting..."

chDayMsg = "Choose the day"

chTimeMsg = "Choose the time book"

invOptMsh = "Invalid option"

rWidgetMsg =  "Welcome to table booking system\n1.Book a table\n" ++ 
              "2.Login to admin panel\n[q] Quit"

adminMsg = "Choose the action:\n1. Book table\n2. Unbook table\n" ++ 
              "3. Init days(will rewrite tables)\n" ++ 
              "4. Add days to end\n5. Show booked tables\n[q] Quit\n[l] Logout"

roleMsg = "Welcome to table booking system\n1.Book a table\n" ++
             "2.Login to admin panel\n[q] Quit"

lastPicksMsg = "\n[b] Go back\n[q] Quit"

checkDataMsg = "Check your data:\n"

phoneErrMsg = "Phone must be a number"
enterPhoneMsg = "Enter phone of person:\n"

enterDaysMsg = "Enter number of days:\n"

passMsg = "Enter the password for admin:\n"

uniquePhoneMsg = "This phone number already exists in database"

contMsg = "Press any key to continue"

wrongPassMsg = "Wrong password!\n"
noDaysMsg = "No availible days. Press any key\n"
