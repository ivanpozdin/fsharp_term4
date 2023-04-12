type rounding(p: int) =
    member this.Bind(x, f) = f x
    member this.Return(x: float) = System.Math.Round(x, p)

let isInt (str: string) =
    (System.Text.RegularExpressions.Regex("^[0-9]+$")).IsMatch str

type calculate() =
    member this.Bind(m, f) =
        match m with
        | "None" -> "None"
        | a when a |> isInt -> f (a |> int)
        | _ -> "None"

    member _.Return(x) = x |> string
