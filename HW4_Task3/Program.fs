module HW4_Task3.User_Interface

open HW4_Task3.PhoneBook

printfn "Phone book has 7 commands, which are numbers from 0 to 6."
printfn "0: finish work with phone book."
printfn "1: add new entry to phone book."
printfn "2: find phone by name in phone book."
printfn "3: find name by phone in phone book."
printfn "4: print phone book."
printfn "5: safe changes to file."
printfn "6: import everything from file."

start [] (getLineFor "Enter file path where to store phone book: ") (getCommand ())
|> ignore
