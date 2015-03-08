open System

let properDivisorsOf n =
    [1 .. (n/2)]
    |> List.filter (fun x -> n % x = 0)

open System.Collections.Generic

let memoize f =
    let dict = Dictionary<_, _>()
    fun x ->
        match dict.TryGetValue(x) with
        | true, res -> res
        | false, _ -> 
            let res = f x
            dict.Add(x, res)
            res

let sumOfProperDivisorsOf =
    memoize (fun x ->
        properDivisorsOf x
        |> List.sum)

let isAmicable n =
    let sum = sumOfProperDivisorsOf n
    let sum' = sumOfProperDivisorsOf sum
    sum <> n && sum' = n
    
let euler021 = 
    [1 .. 9999]
    |> List.filter isAmicable
    |> List.sum
 
Console.WriteLine euler021