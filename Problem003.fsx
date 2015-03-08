open System

let factors number = 
    {2L .. number / 2L}
    |> Seq.filter (fun x -> number % x = 0L)

let factorsDescending number =
    factors number
    |> Seq.map (fun x -> number / x)

let isPrime number =
    let root = float >> sqrt >> int64 <| number
    {2L .. root}
    |> Seq.exists (fun x -> number % x = 0L)
    |> not

let euler003 =
    factorsDescending 600851475143L
    |> Seq.find isPrime

Console.WriteLine euler003