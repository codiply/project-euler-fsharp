open System

let divisibleByAll dividers number =
    dividers
    |> List.exists (fun d -> number % d <> 0)
    |> not

let smallestNumberDivisibleByAll dividers =
    let max = dividers |> List.max
    Seq.initInfinite ((+) 1 >> (*) max)
    |> Seq.find (divisibleByAll dividers)

let euler005 =
    let dividers = [1 .. 20]
    smallestNumberDivisibleByAll dividers

Console.WriteLine euler005