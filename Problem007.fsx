open System

let isPrime number =
    let root = float >> sqrt >> int <| number
    {2 .. root}
    |> Seq.exists (fun x -> number % x = 0)
    |> not

let oddNumbers = 
    Seq.initInfinite ((*) 2 >> (+) 1)

let primes = 
    oddNumbers 
    |> Seq.filter isPrime

let euler007 =
    primes
    |> Seq.skip 10000 
    |> Seq.head

Console.WriteLine euler007