open System.Collections.Generic

let memoize f =
    let dict = Dictionary<_, _>()
    fun x ->
        match dict.TryGetValue(x) with
        | true, res -> res
        | false, _ -> 
            let res = f x
            dict.Add(x, res)
            res

let isPrime =
    let f number = 
        let root = float >> sqrt >> int <| number
        {2 .. root}
        |> Seq.exists (fun x -> number % x = 0)
        |> not
    memoize (fun x -> f x)

let digits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList
    |> List.rev

let fromDigits b digits =
    Seq.unfold(fun x -> Some(x, x * b)) 1
    |> Seq.zip (List.rev digits)
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum

let rotate = function
    | [] -> []
    | x::xs -> xs @ [x]

let rotations lst = 
    [1 .. (List.length lst - 1)]
    |> List.scan (fun s _ -> rotate s) lst

let isCircularPrime n =
    digits 10 n
    |> rotations
    |> List.map (fun x -> fromDigits 10 x)
    |> List.forall isPrime

let euler035 = 
    [2 .. 999999]
    |> List.filter isPrime
    |> List.filter isCircularPrime
    |> List.length

System.Console.WriteLine euler035