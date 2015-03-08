let properDivisorsOf n =
    [1 .. (n/2)]
    |> List.filter (fun x -> n % x = 0)

let isAbundand n =
    (properDivisorsOf n |> List.sum) > n

let abundandNumbers =
    [1 .. 28123]
    |> List.filter isAbundand

let sumsOf2AbundandNumbers =
    [ for i in abundandNumbers do
         for j in (abundandNumbers 
                   |> Seq.skipWhile (fun x -> x < i)
                   |> Seq.takeWhile (fun x -> x + i <= 28123)) do
             yield i + j ]

let euler023 = 
    let sums = new Set<_>(sumsOf2AbundandNumbers)
    [1 .. 28123]
    |> List.filter (fun x -> sums.Contains(x) |> not)
    |> List.sum

System.Console.WriteLine euler023