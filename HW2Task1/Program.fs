module HW2Task1.functions

let evenCount1 ls =
    List.sum (ls |> List.map (fun item -> abs (item + 1) % 2))

let evenCount2 ls =
    List.length (ls |> List.filter (fun item -> abs (item % 2) = 0))

let evenCount3 ls =
    ls |> List.fold (fun count item -> (count + (abs (item + 1) % 2))) 0
