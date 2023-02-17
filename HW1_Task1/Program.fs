// For more information see https://aka.ms/fsharp-console-apps
let rec factorial acc x =
    if x = 1 then acc else factorial (acc * x) (x - 1)

printfn $"{factorial 1 16}"

let fib n =
    let rec fib2 a b n curN =
        if n <= 0 then 0
        else if curN = n then a
        else fib2 (a + b) a n (curN + 1)

    fib2 1 0 n 1

printfn $"Число фибоначчи под номером 40: {fib 40}"
