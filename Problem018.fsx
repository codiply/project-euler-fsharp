open System

let triangleString =    @"  75                             
                          95  64                           
                        17  47  82                         
                      18  35  87  10                       
                    20  04  82  47  65                     
                  19  01  23  75  03  34                   
                88  02  77  73  07  63  67                 
              99  65  04  28  06  16  70  92               
            41  41  26  56  83  40  80  70  33             
          41  48  72  33  47  32  37  16  94  29           
        53  71  44  65  25  43  91  52  97  51  14         
      70  11  33  28  77  73  17  78  39  68  17  57       
    91  71  52  38  17  14  91  43  58  50  27  29  48     
  63  66  04  68  89  53  67  30  73  16  69  87  40  31   
04  62  98  27  23  09  70  98  73  93  38  53  60  04  23"

let triangle =
    triangleString.Split([|'\n'|])
    |> Array.map (fun s -> 
        s.Trim().Split([|' '|])
        |> Array.filter (fun x -> not <| System.String.IsNullOrEmpty(x))
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

let euler018 =
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

Console.WriteLine euler018