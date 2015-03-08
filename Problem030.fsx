let reverseDigits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList

let fifthPower = [| 0 .. 9 |] |> Array.map (fun x -> x * x * x * x * x) 

let sumOf5thPowers number =
    reverseDigits 10 number
    |> Seq.map (fun d -> fifthPower.[d])
    |> Seq.sum

let euler030 = 
    {2 .. 354294}
    |> Seq.filter (fun x -> sumOf5thPowers x = x)
    |> Seq.sum

System.Console.WriteLine euler030