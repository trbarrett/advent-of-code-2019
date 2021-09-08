#load "./Helper.fsx"
open Helper
let numbers = Helper.readInput "day1.txt" |> Seq.map int |> Set
let result =
    numbers
    |> Seq.map (fun x -> (x / 3) - 2)
    |> Seq.sum

printfn $"{result}"