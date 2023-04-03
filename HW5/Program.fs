// For more information see https://aka.ms/fsharp-console-apps
open System

type Computer(os: string, ip: string) =
    let dictOS =
        dict["Windows", 0.7
             "Linux", 0.9
             "MacOS", 0.4]

    member this.OS = os
    member this.ip = ip
    member this.probability = dictOS.Item(os)

let doWithProbability (probability: float) =
    let rnd = new Random()
    let r = rnd.Next(1, 100)

    if (float r) <= probability * (float 100) then
        true
    else
        false

// Функция должна возваратить обновлённый список инфецированных.
// Т.е. Она должна пройт
let rec oneStepInfect (infected: List<Computer>) (neighbours: List<Computer * Computer>) (newInfected: List<Computer>) =
    if neighbours = [] then
        newInfected
    else
        match neighbours.Head with
        | a, b when
            List.contains a infected
            && not (List.contains b infected)
            && (doWithProbability b.probability)
            ->
            oneStepInfect infected neighbours.Tail (b :: newInfected)
        | b, a when
            List.contains a infected
            && not (List.contains b infected)
            && (doWithProbability b.probability)
            ->
            oneStepInfect infected neighbours.Tail (b :: newInfected)
        | _, _ -> oneStepInfect infected neighbours.Tail newInfected

let rec printElements (ls: List<Computer>) =
    if ls = [] then
        printfn ""
    else
        printfn $"{ls.Head.ip}"
        printElements ls.Tail



let rec simulate (infected: List<Computer>) (neighbours: List<Computer * Computer>) step allComputer =
    System.Threading.Thread.Sleep(1500)
    let newInfected = infected @ (oneStepInfect infected neighbours [])
    printfn $"Infected computers on step: {step}"
    printElements newInfected

    if newInfected <> allComputer then
        simulate newInfected neighbours (step + 1) allComputer

let c1 = Computer("Linux", "1")
let c2 = Computer("Linux", "2")
let c3 = Computer("Windows", "3")
let c4 = Computer("MacOS", "4")
let c5 = Computer("Linux", "5")

let n = [ c1, c2; c2, c3; c3, c4; c4, c5 ]

simulate [ c1 ] n 1 [ c1; c2; c3; c4; c5 ]
