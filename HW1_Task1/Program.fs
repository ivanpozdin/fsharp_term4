
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
let reverse ls=
    let rec reverseRec newLs ls i len=
        if i=len then newLs
        else reverseRec (List.head ls :: newLs) (List.tail ls) (i+1) len
    reverseRec [] ls 0 ls.Length
printf $"{reverse [1;2;3;4;5]}"
    
    
