let revDigits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList

let factorial x =
    let rec loop x acc =
        if x <= 1 then acc
        else loop (x - 1) (acc * x)
    loop x 1

let factorials = [| 0 .. 9 |] |> Array.map factorial

let isCurious x =
    (revDigits 10 x
    |> List.map (fun d -> factorials.[d])
    |> List.sum) = x

let euler034 = 
    {3 .. 10000000}
    |> Seq.filter isCurious
    |> Seq.sum
    
System.Console.WriteLine euler034