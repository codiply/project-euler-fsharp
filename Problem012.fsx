open System

let triangleNumbers = 
    Seq.initInfinite ((+) 2)
    |> Seq.scan (+) 1

let countDivisors number =
    let root = float >> sqrt >> int <| number
    let count =
        {2 .. (root - 1)}
        |> Seq.filter (fun x -> number % x = 0)
        |> Seq.length
        |> ((*) 2)
    if root * root = number
    then count + 1
    else count 

let euler012 =
    triangleNumbers
    |> Seq.find (fun x -> countDivisors x > 500)

Console.WriteLine euler012