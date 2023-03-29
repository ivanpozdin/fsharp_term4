module HW4_Task3.PhoneBook

open System
open Newtonsoft.Json
open System.IO

type Entry = { Name: string; Phone: string }

let addEntryToList name phone ls = { Name = name; Phone = phone } :: ls

let getEntryListFromFile fileName =
    File.ReadAllText(fileName) |> JsonConvert.DeserializeObject<List<Entry>>

let getName () = Console.ReadLine()

let getFileName () = Console.ReadLine()

let getCommand () =
    Console.WriteLine("Enter command:")
    Console.ReadLine() |> int

let getEntry () =
    Console.WriteLine("Enter name:")
    let name = Console.ReadLine()
    Console.WriteLine("Enter phone:")
    let phone = Console.ReadLine()
    { Name = name; Phone = phone }

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


let rec start command (data: List<Entry>) fileName =
    match command with
    | 0 -> null
    | 1 -> start (getCommand ()) (getEntry () :: data) fileName
    | 2 ->
        printfn $"{findPhoneByName data (getName ())}"
        start (getCommand ()) data fileName
    | 3 ->
        printfn $"{findNameByPhone data (getName ())}"
        start (getCommand ()) data fileName
    | 4 ->
        printPhoneBook data
        start (getCommand ()) data fileName
    | 5 ->
        let json = JsonConvert.SerializeObject data
        File.WriteAllText(fileName, json)
        start (getCommand ()) data fileName
    | 6 -> start (getCommand ()) (getEntryListFromFile fileName) fileName
