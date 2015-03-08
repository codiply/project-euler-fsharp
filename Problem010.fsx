open System

let isPrime number =
    let root = float >> sqrt >> int64 <| number
    {2L .. root}
    |> Seq.exists (fun x -> number % x = 0L)
    |> not

let euler010 = 
    {2L .. 2000000L}
    |> Seq.filter isPrime
    |> Seq.reduce (+)

Console.WriteLine euler010