open System

let euler006 =
    {1 .. 100}
    |> Seq.fold (fun (s, s2) i -> (s + i, s2 + i * i)) (0, 0)
    |> (fun (s, s2) -> s * s - s2)

Console.WriteLine euler006