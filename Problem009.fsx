open System

let isPythagorean (a, b, c) =
    a * a + b * b = c * c

let tripletsWithSum sum =
    seq {
        for c in [(sum / 3) + 1 .. (sum - 3)] do
            for b in [2 .. (c - 1)] do
                let a = sum - b - c in
                if a > 0 && a < b then
                    yield (a, b, c)
    }

let euler009 =
    tripletsWithSum 1000
    |> Seq.filter isPythagorean
    |> Seq.toList
    |> (fun lst -> 
            (lst, 
             lst |> List.head |> (fun (a, b, c) -> a * b * c)))

Console.WriteLine euler009