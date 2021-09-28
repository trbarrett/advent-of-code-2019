open System

#load "./Helper.fsx"

// Day 8 - Part 1 is about counting occurrences
//       - Part 2 is about merging layers, and displaying a simple image
//
// Method:
//       - Part 1 we count occurrences of each number and put them into an array
//       - Part 2 we combine each image with the one below via a reduce function

open Helper

let splitIntoLayers (width,height)  =
    List.chunkBySize (width * height)

let countOccurrences xs = Seq.countBy id xs

let convertCountsToArrayBuckets numberOfBuckets xs =
    let a = Array.create numberOfBuckets 0
    xs |> Seq.iter (fun (i,n) -> a.[i] <- n )
    a

let findMinZeros (xs : seq<int * int []>) =
    xs |> Seq.minBy (fun (i, arr) -> arr.[0])

let part1 input size colourSpace =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let layersWithCounts =
        input
        |> List.ofSeq
        |> splitIntoLayers size
        |> Seq.map countOccurrences
        |> Seq.map (convertCountsToArrayBuckets colourSpace)
        |> Seq.indexed

    let layer, n1s, n2s, result =
        layersWithCounts
        |> findMinZeros
        |> fun (i, xs) -> i, xs.[1], xs.[2], (xs.[1] * xs.[2])

    printfn $"Part1 result: {result} for layer: {layer + 1}, took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: 1920 for layer: 10

let part2 input ((width, height) as size) =
    let combineLayers topLayer nextLayer =
        (topLayer, nextLayer)
        ||> List.map2 (fun x y -> if x = 2 then y else x)

    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let layers =
        input
        |> List.ofSeq
        |> splitIntoLayers size

    let combinedImage =
        List.reduce combineLayers layers

    printfn $"Part2 result image below, took: {sw.ElapsedMilliseconds}ms\n"
    combinedImage
    |> List.map (fun x -> if x = 1 then "*" else " ")
    |> List.chunkBySize (width)
    |> List.iter (fun row -> printfn "%s" (String.Join("",row)))
    // Correct Answer: "PCULA"

let input =
    Helper.readUncommentedLines "day08.txt"
    |> Seq.item 0
    |> Seq.map (fun ch -> ch |> string |> int)


//part1 input (3,2) 10 // example 1
//part2 input (2,2) // example 2
part1 input (25,6) 3
part2 input (25,6)
