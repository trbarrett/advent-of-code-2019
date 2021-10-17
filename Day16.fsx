#load "./Helper.fsx"

// Day 16 - Numerical shuffling

open Helper

let basePattern = [|0L; 1L; 0L; -1L|]
let basePatternLength = Seq.length basePattern

let indexPattern n =
    Seq.initInfinite (fun i -> basePattern.[(i/n) % basePatternLength])
    |> Seq.skip 1

let getIterationValue input =
        [1..(Seq.length input)]
        |> List.map (fun i ->
            Seq.zip input (indexPattern i)
            |> Seq.sumBy (fun (a,b) -> a * b)
            |> (fun x -> (abs x) % 10L))
        //|> tee (fun x -> printfn "%A" (System.String.Join("", x)))

let performIterations input n =
    let f = List.replicate n getIterationValue |> List.reduce (>>)
    f input

let input =
    Helper.readLinesWithSlashComments "day16.txt"
    |> Seq.item 0
    |> Seq.map (Char.digitToInt >> int64)
    |> Seq.toList

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let result =
        performIterations input 100
        |> List.take 8
    let out = System.String.Join("", result)
    printfn $"Part1 result: {out}, took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: 78009100, took: 5567ms

part1 ()
