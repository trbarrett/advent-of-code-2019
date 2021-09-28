#load "./Helper.fsx"

// Day 3 - Finding path intersection points
//
// Method - Brute force search of cross-join of all potential line segments
// Comments - With the limited domain, using a hashtable of every point in a
//            line might have been a faster and shorter implementation.
//
//            If the wires were an order of magnitude larger we could have used
//            a partitioning system so we didn't need to cross compare all
//            segments

open Helper

type PathVector = | D of int | U of int | L of int | R of int

module PathVector =
    let parse (segmentStr: string) =
        match segmentStr.[0] with
        | 'D' -> D | 'U' -> U | 'L' -> L | 'R' -> R
        |> fun c -> c (int (segmentStr.Substring 1))

type Range1D = { P1 : int; P2 : int }

module Range1D =
    let create p1 p2 =
        if p1 > p2
        then { P1 = p2; P2 = p1 }
        else { P1 = p1; P2 = p2 }

    let hasIntersection r1 r2 =
        (r1.P2 < r2.P1 || r1.P1 > r2.P2) |> not

type Axis = | NS | EW

type PathSegment =
    { X1 : int; Y1 : int; X2 : int; Y2 : int; Axis : Axis }

module PathSegment =
    let intersection a b =
        if a.Axis = b.Axis then None
        elif Range1D.hasIntersection (Range1D.create a.X1 a.X2)
                                     (Range1D.create b.X1 b.X2)
          && Range1D.hasIntersection (Range1D.create a.Y1 a.Y2)
                                     (Range1D.create b.Y1 b.Y2)
        then
            match a.Axis with
            | NS -> Some (a.X1, b.Y1)
            | EW -> Some (b.X1, a.Y1)
        else None

    let length segment =
        match segment.Axis with
        | NS -> segment.Y2 - segment.Y1 |> abs
        | EW -> segment.X2 - segment.X1 |> abs


let rec vectorsToSegments' vectors (x1, y1) acc =
    let createSegment x2 y2 axis =
        { X1 = x1; Y1 = y1; X2 = x2; Y2 = y2; Axis = axis }
    match vectors with
    | [] -> acc
    | head::tail ->
        let newSegment =
            match head with
            | D n -> createSegment x1 (y1 - n) NS
            | U n -> createSegment x1 (y1 + n) NS
            | L n -> createSegment (x1 - n) y1 EW
            | R n -> createSegment (x1 + n) y1 EW
        vectorsToSegments' tail (newSegment.X2, newSegment.Y2) (newSegment::acc)

let vectorsToSegments vectors =
    let segmentsRev = vectorsToSegments' (Seq.toList vectors) (0, 0) []
    List.rev segmentsRev

let findCrossingPoint wireA wireB =
    seq { for segA in wireA do
              for segB in wireB do
                  PathSegment.intersection segA segB }
    |> Seq.choose id
    |> Seq.map (fun (x, y) -> abs x + abs y)
    |> Seq.filter (fun n -> n > 0)
    |> Seq.min

let getSteps wire segmentNo (x, y) =
    let length =
        wire
        |> Seq.take segmentNo
        |> Seq.map PathSegment.length
        |> Seq.sum

    let lastSegment = Seq.item segmentNo wire
    let finalLength =
        match lastSegment.Axis with
        | NS -> abs (y - lastSegment.Y1)
        | EW -> abs (x - lastSegment.X1)

    length + finalLength

let findEarliestCrossingPoint wireA wireB =
    seq { for segAi, segA in Seq.indexed(wireA) do
              for segBi, segB in Seq.indexed(wireB) do
                  PathSegment.intersection segA segB
                  |> Option.map (fun (x, y) -> x, y, segAi, segBi) }
    |> Seq.choose id
    |> Seq.map (fun (x, y, segAi, segBi) ->
        //printfn $"Crossing Point x,y: {(x,y)} at wire {segAi} and {segBi}"
        getSteps wireA segAi (x,y) + getSteps wireB segBi (x,y))
    |> Seq.filter (fun n -> n > 0)
    |> Seq.min

let sw = System.Diagnostics.Stopwatch.StartNew ()
let wireA, wireB =
    Helper.readInput "day03.txt"
    |> Seq.map (String.split ',')
    |> Seq.map (Seq.map PathVector.parse)
    |> Seq.map vectorsToSegments
    |> fun xs -> (Seq.item 0 xs, Seq.item 1 xs)
printfn $"Setup Took: {sw.ElapsedMilliseconds}ms"

sw = System.Diagnostics.Stopwatch.StartNew ()
printfn $"Part 1: {findCrossingPoint wireA wireB}, took: {sw.ElapsedMilliseconds}ms"

sw = System.Diagnostics.Stopwatch.StartNew ()
printfn $"Part 2: {findEarliestCrossingPoint wireA wireB}, took: {sw.ElapsedMilliseconds}ms"
