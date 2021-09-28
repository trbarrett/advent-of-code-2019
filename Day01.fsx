#load "./Helper.fsx"

let numbers = Helper.readInput "day01.txt" |> Seq.map int |> Set

let rec extraFuelCalc fuel acc =
    if fuel <= 0
    then acc
    else
        let extra = (fuel / 3) - 2
        let extra = System.Math.Max(extra, 0)
        extraFuelCalc extra (acc + extra)

let part1 =
    numbers
    |> Seq.map (fun x -> (x / 3) - 2)
    |> Seq.sum

let part2 =
    numbers
    |> Seq.map (fun x -> (x / 3) - 2)
    |> Seq.map (fun x -> x + extraFuelCalc x 0)
    |> Seq.sum

printfn $"Part 1: {part1}"
printfn $"Part 2: {part2}"