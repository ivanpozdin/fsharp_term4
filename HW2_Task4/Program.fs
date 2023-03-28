let isPrime (n: bigint) =
    let rec loop i =
        if i * i > n then true
        elif n % i = 0I then false
        else loop (i + 1I)

    if n < 2I then false else loop 2I

let primes =
    let unfoldNextPrime n =
        let rec findNextPrimeRec m =
            if isPrime m then m else findNextPrimeRec (m + 1I)

        let nextPrime = findNextPrimeRec (n + 2I)
        Some(nextPrime, nextPrime)

    Seq.unfold unfoldNextPrime 2I |> Seq.append { 2I .. 3I }

primes |> Seq.take 150 |> Seq.iter (printfn "%A")
