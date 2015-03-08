open System

let routesToNextNode (knownRoutes : Map<int64 * int64, int64>) (i, j) =
    let routes i j = Map.find (i, j) knownRoutes
    let result =
        // this is the starting point (0,0)
        if i + j = 0L then 1L
        else
            // If we are on the edge of the grid, there is 
            // only one previous node rather than two
            (if i > 0L then routes (i-1L) j else 0L) +
            (if j > 0L then routes i (j-1L) else 0L)
    Map.add (i, j) result knownRoutes
    

let routesToNode (m, n) = 
    [ for i in [0L .. m] do
          for j in [0L .. n] do
              yield (i, j) ]
    |> List.fold routesToNextNode Map.empty
    |> Map.find (m, n)

let euler015 = routesToNode (20L, 20L)

Console.WriteLine euler015