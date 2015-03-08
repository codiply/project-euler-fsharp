open System

let rec tell = function
    | 0 -> ""
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four" 
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen" 
    | x when x < 30 -> "twenty" + (if x <> 20 then "-" else "") + tell (x - 20)
    | x when x < 40 -> "thirty" + (if x <> 30 then "-" else "") + tell (x - 30)
    | x when x < 50 -> "forty" + (if x <> 40 then "-" else "") + tell (x - 40)
    | x when x < 60 -> "fifty" + (if x <> 50 then "-" else "") + tell (x - 50)
    | x when x < 70 -> "sixty" + (if x <> 60 then "-" else "") + tell (x - 60)
    | x when x < 80 -> "seventy" + (if x <> 70 then "-" else "") + tell (x - 70)
    | x when x < 90 -> "eighty"  + (if x <> 80 then "-" else "") + tell (x - 80)
    | x when x < 100 -> "ninety" + (if x <> 90 then "-" else "") + tell (x - 90)
    | x when x < 1000 -> 
        let h = x / 100 
        let r = x % 100 in
        tell h + " hundred" + (if r = 0 then "" else " and ") + tell (x % 100)
    | 1000 -> "one thousand"
    | _ -> ""

let countLetters s =
    [for c in s -> c]
    |> List.filter (fun x -> x <> '-' && x <> ' ')
    |> List.length

let euler017 =
    [1 .. 1000]
    |> List.map tell
    |> List.map countLetters
    |> List.sum

Console.WriteLine euler017