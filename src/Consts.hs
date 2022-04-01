module Consts
    (
       oneM,
       oneH,
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
       noDaysMsg
     ) where

oneM = 60
oneH = oneM * 60

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
              "4. Add days to end\n[q] Logout"

roleMsg = "Welcome to table booking system\n1.Book a table\n" ++
             "2.Login to admin panel\n[q] Quit"

lastPicksMsg = "\n[b]. Go back\n[q].Quit"

checkDataMsg = "Check your data:\n"

enterPhoneMsg = "Enter phone of person:\n"

enterDaysMsg = "Enter number of days:\n"

noDaysMsg = "No availible days. Press any key\n"
