module Tests

open Xunit
open HW5.Virus_Simulation

[<Fact>]
let ``Probability of infection: zero`` () =

    let c1 = Computer("Linux", "1")
    let c2 = Computer("NoToVirus", "2")
    let c3 = Computer("Windows", "3")
    let c4 = Computer("MacOS", "4")
    let c5 = Computer("Linux", "5")
    let infected = [ c1 ]
    let neighbours = [ c1, c2; c2, c3; c3, c4; c4, c5 ]
    let newInfected = oneStepInfect infected neighbours []
    let p = (newInfected = [])
    Assert.True p

[<Fact>]
let ``Probability of infection: one`` () =

    let c1 = Computer("YesToVirus", "1")
    let c2 = Computer("YesToVirus", "2")
    let c3 = Computer("YesToVirus", "3")
    let c4 = Computer("YesToVirus", "4")
    let c5 = Computer("YesToVirus", "5")
    let c6 = Computer("YesToVirus", "6")
    let c7 = Computer("YesToVirus", "7")
    let c8 = Computer("YesToVirus", "8")
    let c9 = Computer("YesToVirus", "9")
    let c10 = Computer("YesToVirus", "10")
    let infected = [ c1 ]

    let neighbours =
        [ c1, c2; c1, c3; c2, c4; c2, c5; c3, c6; c4, c7; c5, c8; c6, c9; c6, c10 ]

    let newInfected1 = oneStepInfect infected neighbours []
    let infected1 = infected @ newInfected1
    let newInfected2 = oneStepInfect infected1 neighbours []
    let infected2 = infected1 @ newInfected2
    let newInfected3 = oneStepInfect infected2 neighbours []

    let p1 = (newInfected1 = [ c3; c2 ])
    let p2 = (newInfected2 = [ c6; c5; c4 ])
    let p3 = (newInfected3 = [ c10; c9; c8; c7 ])
    Assert.True(p1 && p2 && p3)
