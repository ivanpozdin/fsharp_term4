module HW4_Task3.PhoneBook

open System
open Newtonsoft.Json
open System.IO

type Entry = { Name: string; Phone: string }

let addEntryToList name phone ls = { Name = name; Phone = phone } :: ls

let getEntryListFromFile filePath =
    File.ReadAllText(filePath) |> JsonConvert.DeserializeObject<List<Entry>>

let savePhoneBookToFile (data: List<Entry>) filePath =
    let json = JsonConvert.SerializeObject data
    File.WriteAllText(filePath, json)

let getLineFor (message: string) =
    printfn $"{message}"
    Console.ReadLine()

let getCommand () = getLineFor "Enter command: " |> int

let getEntry () =
    { Name = (getLineFor "Enter name: ")
      Phone = (getLineFor "Enter phone: ") }

let rec findPhoneByName (data: List<Entry>) name =
    if data = [] then ""
    else if data.Head.Name = name then data.Head.Phone
    else findPhoneByName data.Tail name

let rec findNameByPhone (data: List<Entry>) phone =
    if data = [] then ""
    else if data.Head.Phone = phone then data.Head.Name
    else findNameByPhone data.Tail phone

let rec printPhoneBook (data: List<Entry>) =
    if data = [] then
        printf ""
    else
        printfn $"{data.Head.Name}: {data.Head.Phone}"
        printPhoneBook data.Tail


let rec start (data: List<Entry>) filePath command =
    match command with
    | 0 -> null
    | 1 -> start (getEntry () :: data) filePath (getCommand ())
    | 2 ->
        let name = getLineFor "Enter name: "
        printfn $"{findPhoneByName data name}"
        start data filePath (getCommand ())
    | 3 ->
        let phone = getLineFor "Enter phone: "
        printfn $"{findNameByPhone data phone}"
        start data filePath (getCommand ())
    | 4 ->
        printPhoneBook data
        start data filePath (getCommand ())
    | 5 ->
        savePhoneBookToFile data filePath
        start data filePath (getCommand ())
    | 6 -> start (getEntryListFromFile filePath) filePath (getCommand ())
    | _ -> start data filePath (getCommand ())
