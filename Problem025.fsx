let zipAll alst blst d =
    let rec loop alst blst revAcc =
        match alst, blst with
        | a::atl, b::btl -> loop atl btl ((a,b)::revAcc)
        | a::atl, [] -> loop atl [] ((a,d)::revAcc)
        | [], b::btl -> loop [] btl ((d,b)::revAcc)
        | _ -> revAcc |> List.rev
    loop alst blst []

let add alst blst =
    let divmod num div = (num / div, num % div)
    zipAll (alst |> List.rev) (blst |> List.rev) 0
    |> List.fold (fun (carry, result) (a, b) ->
        let sum = a + b + carry
        let (carry, digit) = divmod sum 10
        (carry, digit::result))
        (0, [])
    |> (fun (carry, digits) ->
        if carry > 0 then carry::digits else digits)

let fibonacciSeq =
    Seq.unfold (fun (a, b) -> Some( a, (b, add a  b) ) ) ([1], [1])

let euler025 = 
    fibonacciSeq 
    |> Seq.mapi (fun i n -> (i + 1, n))
    |> Seq.skipWhile (fun (i, n) -> n |> List.length < 1000)
    |> Seq.head
    |> fst

System.Console.WriteLine euler025