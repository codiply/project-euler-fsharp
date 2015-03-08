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

let isPrime =
    let f number = 
        let root = float >> sqrt >> int <| number
        {2 .. root}
        |> Seq.exists (fun x -> number % x = 0)
        |> not
    memoize (fun x -> f (abs x))

open System.Collections.Concurrent

let memoizeConcurrent f =
    let dict = ConcurrentDictionary<_,_>()
    fun x -> dict.GetOrAdd(x, lazy (f x)).Force()

let isPrimeConcurrent =
    let f number = 
        let root = float >> sqrt >> int <| number
        {2 .. root}
        |> Seq.exists (fun x -> number % x = 0)
        |> not
    memoizeConcurrent (fun x -> f (abs x))

let quadratic a b =
    (fun (n : int) -> n * n + a * n + b)

let countConsecutivePrimes (f : int -> int) =
    Seq.unfold (fun x -> Some(x, x+1)) 0
    |> Seq.takeWhile (fun x -> f x |> isPrime)
    |> Seq.length

let countConsecutivePrimesConcurrent (f : int -> int) =
    Seq.unfold (fun x -> Some(x, x+1)) 0
    |> Seq.takeWhile (fun x -> f x |> isPrimeConcurrent)
    |> Seq.length

let result() = 
    [| for a in [|-999 .. 999|] do
           for b in ([|-999 .. 999 |] |> Array.filter isPrime) do 
               yield (a,b) |]
    |> Array.map (fun (a,b) -> ((a,b), quadratic a b |> countConsecutivePrimes))
    |> Array.maxBy snd
    |> (fun ((a,b), count) -> ((a,b), count,a*b))

let resultParallel() = 
    [| for a in [|-999 .. 999|] do
           for b in ([|-999 .. 999|] |> Array.filter isPrime) do 
               yield (a,b) |]
    |> Array.Parallel.map (fun (a,b) -> ((a,b), quadratic a b |> countConsecutivePrimesConcurrent))
    |> Array.maxBy snd
    |> (fun ((a,b), count) -> ((a,b), count, a*b))

let timedFunc f =
      let sw = System.Diagnostics.Stopwatch.StartNew()
      let res = f()
      sw.Stop()
      (sw.Elapsed, res)

let euler027 = timedFunc result

let euler027parallel = timedFunc resultParallel