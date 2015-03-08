open System.Numerics

let power (a : int, b : int) =
    let bigA = BigInteger(a)
    let rec loop a b acc =
        if b <= 0 then acc
        else loop a (b - 1) (acc * bigA)
    loop a b (BigInteger(1))

let euler029 = 
    seq { for a in [2 .. 100] do
              for b in [2 .. 100] do
                  yield (a, b) } 
    |> Seq.distinctBy power
    |> Seq.length

System.Console.WriteLine euler029