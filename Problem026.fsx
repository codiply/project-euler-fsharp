let division n d =
    Seq.unfold (fun x -> 
        Some(x / d, 10 * (x % d))) n

let remainders n d =
    Seq.unfold (fun x -> 
        let rem = x % d in
        Some(rem, 10 * rem)) n
    |> Seq.takeWhile (fun x -> x <> 0)

let cycleLength sequence =
    let rec loop sequence pastValues currPos =
        if Seq.isEmpty sequence then
            None
        else
            let elem = Seq.head sequence
            if Map.containsKey elem pastValues then
                let prevPos = Map.find elem pastValues
                Some(currPos - prevPos)
            else
                let sequence = Seq.skip 1 sequence
                let pastValues = Map.add elem currPos pastValues
                let currPos = currPos + 1
                loop sequence pastValues currPos
    loop sequence Map.empty 0


let recurringCycleLength n d =
    remainders n d
    |> cycleLength

let euler026 = 
    [| 1 .. 999 |]
    |> Array.Parallel.map (fun x -> (x, recurringCycleLength 1 x))
    |> Array.maxBy snd
    
System.Console.WriteLine euler026