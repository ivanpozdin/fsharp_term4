// 1. Реализовать функцию вычисления факториала.
printfn "ЗАДАНИЕ1"

let rec factorial acc x =
    if x = 1 then acc else factorial (acc * x) (x - 1)

printfn $"Факториал числа 16:  {factorial 1 16}"
printfn ""


// 2. Реализовать функцию вычисления числа Фибоначчи (за линейное время).
printfn "ЗАДАНИЕ2"

let fib n =
    let rec fib2 a b n curN =
        if n <= 0 then 0
        else if curN = n then a
        else fib2 (a + b) a n (curN + 1)

    fib2 1 0 n 1

printfn $"Число Фибоначчи под номером 40:  {fib 40}"
printfn ""


// 3. Реализовать функцию обращения списка (за линейное время).
printfn "ЗАДАНИЕ3"

let reverse ls =
    let rec reverseRec newLs ls i len =
        if i = len then
            newLs
        else
            reverseRec (List.head ls :: newLs) (List.tail ls) (i + 1) len

    reverseRec [] ls 0 ls.Length

printf "Обращение списка [1;2;3;4;5]:  "
reverse [ 1; 2; 3; 4; 5 ] |> List.iter (printf "%d ")
printfn "\n"


// 4. Реализовать функцию, принимающую на вход n и m и возвращающую список из эл-тов [2^n; 2^(n + 1); ...; 2^(n + m)].
printfn "ЗАДАНИЕ4"

let rec powersOfTwo t acc n =
    if t > n then
        acc
    else
        powersOfTwo (t + 1) (acc @ [ acc.[t / 2] * acc.[t / 2] * (1 + (if (t % 2 <> 0) then 1 else 0)) ]) n

let rec getRequestedList t acc n m (ls: _ list) =
    if t > m then
        acc
    else
        getRequestedList (t + 1) (acc @ [ ls.[n] * ls.[t] ]) n m ls

let listNM n m =
    getRequestedList 0 [] n m (powersOfTwo 1 [ 1 ] (max n m))

printf "n=3 m=7 список из эл-тов [2^n; 2^(n + 1); ...; 2^(n + m)]:  "
(listNM 3 7) |> List.iter (printf "%d ")
printfn "\n"


// 5. Реализовать функцию, выдаёющую 1 позицию вхождения заданного числа в список.
printfn "ЗАДАНИЕ5"

let rec firstPosition t num (ls: _ list) =
    if t >= List.length ls then -1
    else if ls.[t] = num then t
    else firstPosition (t + 1) num ls

printfn $"Позиция числа 5 в списке [1;2;3;0;0;7;2;0;735]:  {firstPosition 0 5 [ 1; 2; 3; 0; 0; 7; 2; 0; 735 ]}"
printfn $"Позиция числа 5 в списке [1;2;3;0;5;7;2;0;735]:  {firstPosition 0 5 [ 1; 2; 3; 0; 5; 7; 2; 0; 735 ]}"
