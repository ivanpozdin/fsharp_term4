module Tests

open Xunit
open HW4_Task3.PhoneBook

[<Fact>]
let ``Find phone`` () =
    let list =
        [ { Name = "A"; Phone = "1" }
          { Name = "C"; Phone = "3" }
          { Name = "B"; Phone = "2" } ]

    let result = findPhoneByName list "A"
    Assert.Equal("1", result)

[<Fact>]
let ``Find phone of not added person`` () =
    let list =
        [ { Name = "A"; Phone = "1" }
          { Name = "C"; Phone = "3" }
          { Name = "B"; Phone = "2" } ]

    let result = findPhoneByName list "D"
    Assert.Equal("", result)

[<Fact>]
let ``Find name`` () =
    let list =
        [ { Name = "A"; Phone = "1" }
          { Name = "C"; Phone = "3" }
          { Name = "B"; Phone = "2" } ]

    let result = findNameByPhone list "3"
    Assert.Equal("C", result)

[<Fact>]
let ``Find name of not added phone`` () =
    let list =
        [ { Name = "A"; Phone = "1" }
          { Name = "C"; Phone = "3" }
          { Name = "B"; Phone = "2" } ]

    let result = findNameByPhone list "90"
    Assert.Equal("", result)
