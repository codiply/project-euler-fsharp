let names =
    System.IO.File.ReadAllText("Problem022names.txt").Split([|','|])
    |> Array.map (fun x -> x.Trim('"'))
    |> Array.map (fun x -> x.ToLower())

let lowerChar2int (c : char) =
    System.Convert.ToInt32 c - 96

let alphabeticalValue name =
    [for c in name -> c]
    |> List.map lowerChar2int
    |> List.sum

let euler022 = 
    names
    |> Array.sort
    |> Array.mapi (fun i name -> (i + 1) * alphabeticalValue name)
    |> Array.sum

System.Console.WriteLine euler022