let digits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList
    |> List.rev

let isPalindromicInBase b number =
    let digits = digits b number
    List.rev digits = digits

let euler036 = 
    {1 .. 999999}
    |> Seq.filter (isPalindromicInBase 2)
    |> Seq.filter (isPalindromicInBase 10)
    |> Seq.sum

System.Console.WriteLine euler036