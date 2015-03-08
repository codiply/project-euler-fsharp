open System

let fibonacciSeq =
    Seq.unfold (fun (a, b) -> Some( a, (b, a + b) ) ) (1, 2)

let euler002 = 
    fibonacciSeq
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.takeWhile (fun x -> x <= 4000000)
    |> Seq.sum

Console.WriteLine euler002