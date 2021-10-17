#load "./Helper.fsx"

// Day 16 - Numerical shuffling

open Helper

let iterAPos len spacing =
    [|
        let mutable i = 0
        // below is (spacing-1) because we always skip the first value in the
        // first iteration. It's why this is unrolled from the following loop
        i <- i + (spacing-1) // skip over zeros
        let remSpacing = (min (len-1) (i + spacing-1)) - i
        if remSpacing >= 0 then
            for j in 0..remSpacing do
                let x = i + j
                yield x // return each "1"
            i <- i + spacing + spacing // advance by 1s and skip over zeros
            i <- i + spacing  // advance by -1s

        while i < len do
            i <- i + spacing // skip over zeros
            let remSpacing = (min (len-1) (i + spacing-1)) - i
            if remSpacing >= 0 then
                for j in 0..remSpacing do
                    let x = i + j
                    yield x // return each "1"
                i <- i + spacing + spacing // advance by 1s and skip over zeros
                i <- i + spacing  // advance by -1s
    |]

let iterANeg len spacing =
    [|
        let mutable i = 0
        // below is (spacing-1) because we always skip the first value in the
        // first iteration. It's why this is unrolled from the following loop
        i <- i + (spacing-1) // skip over zeros
        i <- i + spacing + spacing // advance by 1s and skip over zeros
        let remSpacing = (min (len-1) (i + spacing-1)) - i
        if remSpacing >= 0 then
            for j in 0..remSpacing do
                let x = i + j
                yield x // return each "-1"
            i <- i + spacing // advance by -1s

        while i < len do
            i <- i + spacing // skip over zeros
            i <- i + spacing + spacing // advance by 1s and skip over zeros
            let remSpacing = (min (len-1) (i + spacing-1)) - i
            if remSpacing >= 0 then
                for j in 0..remSpacing do
                    let x = i + j
                    yield x // return each "-1"
                i <- i + spacing // advance by -1s
    |]

let iterAPosM = memoize2 iterAPos
let iterANegM = memoize2 iterANeg

let getIterationValue (input : int64 []) =
    [|1..(Seq.length input)|]
    |> Array.map (fun i ->
        //printfn $""
        let mutable sum = 0L
        for j in (iterAPosM (Seq.length input) i) do
            sum <- sum + input.[j]

        for j in (iterANegM (Seq.length input) i) do
            sum <- sum - input.[j]
        (abs sum) % 10L)
    //|> tee (fun x -> printfn "%A" (System.String.Join("", x)))

let performIterations input n =
    let f = Array.replicate n getIterationValue |> Array.reduce (>>)
    f input

let input =
    Helper.readLinesWithSlashComments "day16.txt"
    |> Seq.item 0
    |> Seq.map (Char.digitToInt >> int64)
    |> Seq.toArray

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let result =
        performIterations input 100
        |> Array.take 8
    let out = System.String.Join("", result)
    printfn $"Part1 result: {out}, took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: 78009100, took: 67ms


part1 ()