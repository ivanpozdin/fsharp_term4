let reverseBracket ch =
    match ch with
    | '(' -> ')'
    | ')' -> '('
    | '[' -> ']'
    | ']' -> '['
    | '{' -> '}'
    | '}' -> '{'
    | _ -> ' '

let checkBrackets str =
    let openBrackets = [ '['; '{'; '(' ]
    let closeBrackets = [ ']'; '}'; ')' ]

    let makeNewStackAndLs (stack: char list) (ls: char list) =
        match ls.Head with
        | b when List.contains b openBrackets -> (b :: stack, List.tail ls)
        | b when List.contains b closeBrackets && stack <> [] && stack.Head = reverseBracket b ->
            (List.tail stack, List.tail ls)
        | _ -> ([ ')' ], [])

    let rec check (stack, ls: char list) =
        match ls with
        | [] -> stack = []
        | ls -> makeNewStackAndLs stack ls |> check

    check ([], Seq.toList str)
