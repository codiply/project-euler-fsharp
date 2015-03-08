let digits b number  =
    Seq.unfold (function
                    | 0 -> None
                    | x -> Some (x % b, x / b) )
               number
    |> Seq.toList
    |> List.rev

let positions =
    [1 .. 6]
    |> List.scan (fun x _ -> x * 10) 1
    |> List.map ((+) (-1))

let champernowneConstantFractionalDigits =
    Seq.initInfinite ((+) 1)
    |> Seq.collect (digits 10)

let getSequenceElements positions sequence =
    let orderedPositions = 
        positions
        |> Seq.filter (fun x -> x >= 0) 
        |> Seq.distinct
        |> Seq.sort
        |> Seq.toList
    let rec loop positions revAcc current sequence =
        match sequence, positions, current with
        | s, _, _ when s |> Seq.isEmpty -> revAcc |> List.rev
        | _, [], _ -> revAcc |> List.rev
        | _, hd::tl, c when c < hd ->
            let sequence = sequence |> Seq.skip (hd - c)
            let current = hd
            loop positions revAcc current sequence
        | _, phd::ptl, c when c = phd -> 
            let element = sequence |> Seq.head
            let current = current + 1
            let sequence = sequence |> Seq.skip 1
            let revAcc = element :: revAcc
            loop ptl revAcc current sequence
        | _ -> failwith "Unexpected case"
    loop orderedPositions [] 0 sequence

let euler040 = 
    champernowneConstantFractionalDigits
    |> getSequenceElements positions
    |> Seq.fold (*) 1

System.Console.WriteLine euler040