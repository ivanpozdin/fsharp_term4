// 1. Реализовать функцию вычисления факториала.


printfn "ЗАДАНИЕ1"

let factorial n =
    let rec factorialRec acc x =
        if x = 0 then acc else factorialRec (acc * x) (x - 1)

    factorialRec 1 n

printfn $"Факториал числа 0:   {factorial 0}"
printfn $"Факториал числа 16:  {factorial 16}\n"


// 2. Реализовать функцию вычисления числа Фибоначчи (за линейное время).
printfn "ЗАДАНИЕ2"

let fib n =
    let rec fib2 a b n =
        if n = 1 then a else fib2 (a + b) a (n - 1)

    if n <= 0 then None else Some(fib2 1 0 n)

let printFib n =
    match fib n with
    | None -> printfn "n должно быть больше 0."
    | t -> printfn $"Число Фибоначчи под номером {n} равно {t.Value}"

printFib 40
printfn ""

// 3. Реализовать функцию обращения списка (за линейное время).
printfn "ЗАДАНИЕ3"

let reverse ls =
    let rec reverseRec newLs ls i =
        if ls = [] then
            newLs
        else
            reverseRec (List.head ls :: newLs) (List.tail ls) (i + 1)

    reverseRec [] ls 0

printf "Обращение списка [1; 2; 3; 4; 5]:  "
printfn $"%A{reverse [ 1; 2; 3; 4; 5 ]}\n"


// 4. Реализовать функцию, принимающую на вход n и m и возвращающую список из эл-тов [2^n; 2^(n + 1); ...; 2^(n + m)].
printfn "ЗАДАНИЕ4"

let listNM n m =
    let rec listRec ls acc =
        match acc with
        | 1 -> ls
        | _ -> listRec ((List.head ls) * 2 :: ls) (acc - 1)

    if n <= m then
        Some(reverse (listRec [ pown 2 n ] (m - n)))
    else
        None

let printListNM n m =
    match listNM n m with
    | None -> printfn "n обязано быть не больше m."
    | Some t -> printfn $"Список степеней 2 с показателями от {n} до {m}:  %A{t}"

printListNM 10 20
printfn ""

// 5. Реализовать функцию, выдаёющую 1 позицию вхождения заданного числа в список.
printfn "ЗАДАНИЕ5"

let firstPosition num ls =
    let rec firstPositionRec num ls i =
        match ls with
        | [] -> None
        | ls when (num = List.head ls) -> Some(i)
        | _ -> firstPositionRec num (List.tail ls) (i + 1)

    firstPositionRec num ls 0

let printFirstPosition num ls =
    match firstPosition num ls with
    | None -> printfn $"Числа {num} в списке %A{ls} нет."
    | t -> printfn $"Позиция числа {num} в списке %A{ls}: %i{t.Value}."

printFirstPosition 5 [ 1; 2; 3; 0; 5; 7; 2; 0; 735 ]
printFirstPosition 5 [ 1; 2; 3; 0; 267; 7; 2; 0; 735 ]
