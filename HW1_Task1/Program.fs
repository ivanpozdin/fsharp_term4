// For more information see https://aka.ms/fsharp-console-apps
let rec factorial acc x =
    if x = 1 then acc else factorial (acc*x) (x-1)
printfn $"{factorial 1 16}"