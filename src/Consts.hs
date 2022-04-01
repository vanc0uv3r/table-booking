module Consts
    (
       oneM,
       oneH,
       oneD,
       nameMsg,
       phoneMsg,
       personsMsg,
       correctMsg,
       personsErrMsg,
       successMsg,
       quitMsg,
       chDayMsg,
       chTimeMsg,
       invOptMsh,
       rWidgetMsg,
       adminMsg,
       roleMsg,
       lastPicksMsg,
       checkDataMsg,
       enterPhoneMsg,
       enterDaysMsg,
       noDaysMsg,
       passMsg,
       wrongPassMsg,
       phoneErrMsg,
       uniquePhoneMsg,
        contMsg
        ) where

oneM = 60
oneH = oneM * 60
oneD = 24 * oneH

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
