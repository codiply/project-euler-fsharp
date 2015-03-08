let triangle =
    System.IO.File.ReadAllLines("Problem067triangle.txt")
    |> Array.map (fun s -> 
        s.Trim().Split([|' '|])
        |> Array.map (fun x -> System.Int32.Parse(x)))

let maxColumn row = row

let parents (row, column) =
    match column with
    | 0 -> [(row-1, column)]
    | c when 0 < c && c < maxColumn row -> [(row-1,column-1);(row-1,column)]
    | c when c = maxColumn row -> [(row-1,column-1)]
    | _ -> failwith "Node is outside the bounds of the tree"

let children (row, column) = [(row+1,column);(row+1,column+1)]

let findMaxPath (triangle : int [] []) (knownMaxPaths : Map<int * int, int>) (row, column) =
    let thisMaxPath =
        parents (row, column)
        |> List.map (fun x -> knownMaxPaths.Item(x))
        |> List.max
        |> ((+) triangle.[row].[column])
    Map.add (row, column) thisMaxPath knownMaxPaths

let euler067 =
    let height = Array.length triangle 
    seq {
        for i in [1 .. (height - 1)] do
            for j in [0 .. i] do
                yield (i,j)
        }
    |> Seq.fold (findMaxPath triangle) (Map.add (0,0) triangle.[0].[0] Map.empty)
    |> Map.toList
    |> List.filter (fun ((i,j),_) -> i = (height - 1))
    |> List.map snd
    |> List.max

System.Console.WriteLine euler067