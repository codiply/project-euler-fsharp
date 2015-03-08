open System

type Base = int

type Digits = int list

type Number = Base * Digits

let multiply (b, digits) m =
    let revDigits = List.rev digits
    
    let rec loop revDigits carry acc = 
        match revDigits with
        | [] -> 
            if carry = 0 then acc
            else loop [] (carry / b) ((carry % b)::acc)
        | d :: ds ->
            let d' = carry + d * m in
            loop ds (d' / b) ((d' % b)::acc) 

    (b, loop revDigits 0 [])
            
let euler020 = 
    [1..100]
    |> List.fold (fun x i -> multiply x i) (10, [1])
    |> snd
    |> List.sum

Console.WriteLine euler020