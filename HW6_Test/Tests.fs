module Tests

open Xunit


[<Fact>]
let ``rounding1`` () =
    let result =
        Program.rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    Assert.Equal(0.048, result)

[<Fact>]
let ``rounding2`` () =
    let result =
        Program.rounding 5 {
            let! a = 17.0 / 15.0
            let! b = 0.7
            return a * b
        }

    Assert.Equal(0.79333, result)


[<Fact>]
let ``calculate1`` () =
    let result =
        Program.calculate () {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    Assert.Equal("3", result)

[<Fact>]
let ``calculate2`` () =
    let result =
        Program.calculate () {
            let! x = "1"
            let! y = "{Ъ}"
            let z = x + y
            return z
        }

    Assert.Equal("None", result)
