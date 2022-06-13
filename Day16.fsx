#load "./Helper.fsx"

// Day 16 - Numerical shuffling

open Helper

type Range = { From : int; To: int }

let iterAPos len spacing =
    [|
        let mutable i = 0
        // below is (spacing-1) because we always skip the first value in the
        // first iteration. It's why this is unrolled from the following loop
        i <- i + (spacing-1) // skip over zeros
        let remSpacing = (min (len-1) (i + spacing-1)) - i
        if remSpacing >= 0 then
            yield { From = i; To = i + remSpacing }
            i <- i + spacing + spacing // advance by 1s and skip over zeros
            i <- i + spacing  // advance by -1s

        while i < len do
            i <- i + spacing // skip over zeros
            let remSpacing = (min (len-1) (i + spacing-1)) - i
            if remSpacing >= 0 then
                yield { From = i; To = i + remSpacing }
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
            yield { From = i; To = i + remSpacing }
            i <- i + spacing // advance by -1s

        while i < len do
            i <- i + spacing // skip over zeros
            i <- i + spacing + spacing // advance by 1s and skip over zeros
            let remSpacing = (min (len-1) (i + spacing-1)) - i
            if remSpacing >= 0 then
                yield { From = i; To = i + remSpacing }
                i <- i + spacing // advance by -1s
    |]

let iterAPosM = memoize2 iterAPos
let iterANegM = memoize2 iterANeg

let getIterationValue (input : int64 []) =
    [|1..(Seq.length input)|]
    |> Array.map (fun i ->
        //printfn $""
        let mutable sum = 0L
        for r in (iterAPosM (Seq.length input) i) do
            for j in r.From .. r.To do
                sum <- sum + input.[j]

        for r in (iterANegM (Seq.length input) i) do
            for j in r.From .. r.To do
                sum <- sum - input.[j]
        (abs sum) % 10L)
    //|> tee (fun x -> printfn "%A" (System.String.Join("", x)))

let performIterations input n =
    let f = Array.replicate n getIterationValue |> Array.reduce (>>)
    f input

//let rec figureOutNextNeeded (input : int64 []) (positions : int []) len n =
//    if n = 0 then
//        positions
//        |> Array.map (fun p -> input.[p])
//
//    else
//        // What positions do we need to know to get each of these positions?
//        let subPositions =
//            positions
//            |> Array.map (fun pos ->
//                iterAPosM len pos, iterANegM len pos)
//
//        subPositions
//        |> Array.map (fun (poss, negs) ->
//            // for each value in each position we need to recurse to
//            // figure out what values we would have for that position
//            let posValues = figureOutNextNeeded input poss len (n-1)
//            let negValues = figureOutNextNeeded input negs len (n-1)
//            // now that we know what the values are for each position, we can add them
//            // as requried to get the value for this position
//            let sum = (Array.sum posValues) - (Array.sum negValues)
//            (abs sum) % 10L
//        )
//        // now we know know the values for each needed position we are done

let rec figureOutNextNeededTestRound (input : int64 []) (ranges : Range []) len n =
    if n = 0 then
        printfn "Final ranges: %s" (Seq.toString "," (ranges |> Array.map (sprintf "%A")))
    else
        printfn $"n = {n} - Figure out next needed for {Seq.length ranges} ranges"

        // What positions do we need to know to get each of these positions?
        let subRanges =
            ranges
            |> Array.map (fun range ->
                printfn $"n = {n} - Range: %A{range}"
                iterAPosM len range.From, iterANegM len range.From)
                // Do we need to this for each item in the range. We know what the result will be
                //[|range.From .. range.To|]
                //|> Array.map (fun pos -> iterAPosM len pos, iterANegM len pos))

        subRanges
        |> Array.zip ranges
        |> Array.iter (fun (pos, (posRanges, negRanges)) ->
            printfn $"n = {n} - Found {Array.length posRanges} positive ranges and {Array.length negRanges} negative ranges items to derive position: {pos}"
            figureOutNextNeededTestRound input posRanges len (n-1)
            figureOutNextNeededTestRound input negRanges len (n-1)
            )

let calcPart2 (input : int64 []) =
    let offset =
        input
        |> Array.take 7
        |> Math.digitsToInt64
        |> int
    let startingRanges = [| { From = offset; To = offset + 7 } |]
    //figureOutNextNeeded input startingPositions (Array.length input) 100
    figureOutNextNeededTestRound input startingRanges (Array.length input) 2

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

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let input = input |> Array.replicate 10_000 |> Array.collect id
    calcPart2 input
    //let offset = input |> Array.take 7
    //let input = input |> Array.replicate 50 |> Array.collect id
    //let result = performIterations input 2
    //let out = System.String.Join("", offset)
    //printfn $"Part2 result: {out}, took: {sw.ElapsedMilliseconds}ms"
    printfn $"Part2 result took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: ??

// we can sum the whole list (10_000 copies, 3.5mill items) in 10ms (it takes
// 20ms to replicate the list), but currently it takes 900+ms to do it for 1
// case using our method of Neg and Pos

// but even if we could do 10ms for every list. 10ms * 3.5mill = 35_000 seconds. Not fast enough

// try printing out a pattern for the + and - values we use in each iteration,
// perhaps for a length 100 array

// we only need 8 digits for the final output. Can we work backwards from that
// and do less work to get it? - Probably... That might make things a lot faster
// we know we need offset: 5975589 and the 8 following digits for the first list.
// We could find out what values we need to calculate those 8 digits. And so on.
// That's worth doing I think!


// -------------
// It's still too many numbers:
// e.g. to derive position: 5_975_589 requires 524_412 numbers, 5_975_588,5_975_589,...
// Then each of those 524_412 numbers are going to need similar amounts of numbers for each one...
// And doing that 100 times is not sensible.
// But, there's an obvious pattern with numbers this large. There's no
// negatives, and all the positives are contiguous. They are all subsets of
// each other, so we don't have to do the same thing 524_412 times. We just need
// to do it once, and memoize or DP the result.
//
// If we work in ranges for iterAPos and iterANeg then it might makes things
// simpler. Also, if we always assume we're working at the second half of the
// range we can make a lot of assumptions on how to calculate the numbers.
//
// Still, if we're going down 100 levels there's a bunch of calculations we will
// need to perform.
//
// I need to write down the pattern that's happening in the second half of the
// list, and derive some calculations from that. By that I mean, for a range:
// { From = 5975588; To = 5975596 }, what are the calcs needed for each
// individual item. From there I can encode that pattern.
//
// Then I need to figure out what to do with multiple ranges. If they also have
// an obvious pattern, and are subsets, we can figure that out quite quick.
//
// Anyway to figure out this pattern I want a way of neatly displaying the
// iterAPos/iterANeg results into a file or something with everything neatly
// lined up.


//part1 ()
part2 ()