open System

let collatzNext n =
    if n % 2L = 0L
    then n / 2L
    else 3L * n + 1L

let seqLength knownLengths start =
    let rec step start count =
        if Map.containsKey start knownLengths
        then count + Map.find start knownLengths
        else step (collatzNext start) (count + 1L)
    step start 0L

let findSeqLength knownLengths start =
    let length = seqLength knownLengths start
    Map.add start length knownLengths

let baseCase = Map.add 1L 1L Map.empty

let euler014 =
    {1L .. 999999L}
    |> Seq.fold findSeqLength baseCase
    |> Map.toSeq
    |> Seq.maxBy snd
    

Console.WriteLine euler014