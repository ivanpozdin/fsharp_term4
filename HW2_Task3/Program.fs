type arithmeticExpression =
    | One
    | Sum of arithmeticExpression * arithmeticExpression
    | Mul of arithmeticExpression * arithmeticExpression
    | Minus of arithmeticExpression

let rec eval (a: arithmeticExpression) =
    match a with
    | One -> 1
    | Sum (a1, a2) -> eval a1 + eval a2
    | Mul (a1, a2) -> eval a1 * eval a2
    | Minus (a) -> -(eval a)

printfn "%A"
<| eval (Mul(Sum(One, One), Mul(Sum(Minus(One), Minus(One)), (Sum(One, One)))))
