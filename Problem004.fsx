open System

let reverseDigits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList

let isPalindrome number =
    let revDigs = reverseDigits 10 number
    (=) revDigs <| List.rev revDigs

let pairsDescending lower upper =
    seq {
        for i in [upper .. -1 .. lower] do
            for j in [i .. -1 .. lower] do
                yield (i, j)
    }

let euler004 = 
    pairsDescending 900 999
    |> Seq.map (fun (a, b) -> ((a, b), a * b))
    |> Seq.filter (fun (_, prod) -> isPalindrome prod)
    |> Seq.maxBy snd

Console.WriteLine euler004