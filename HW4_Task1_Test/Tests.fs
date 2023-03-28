module Tests

open Xunit

[<Fact>]
let ``test 1`` () = Assert.True(Program.checkBrackets "()")

[<Fact>]
let ``test 2`` () = Assert.False(Program.checkBrackets ")")

[<Fact>]
let ``test 3`` () =
    Assert.True(Program.checkBrackets "[{()}][]")

[<Fact>]
let ``test 4`` () =
    Assert.False(Program.checkBrackets "[({{}})[()]")

[<Fact>]
let ``test 5`` () =
    Assert.False(Program.checkBrackets "(()}")
