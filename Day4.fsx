#load "./Helper.fsx"

// Day 4 - Secure Container
//         Generating valid passwords
//
// Method: Not quite brute force, but nothing particularly smart. Steps through
//         each position in the number and recurses; counting up the valid cases
//         It probably would have been a lot easier to just do a brute force
//         approach

open Helper
open System

type MatchedAdjacent =
    | NoMatch
    | SeqTooLong of int
    | CurrentlyAdjacent of int
    | LockedInMatch

type Part = | Part1 | Part2

module MatchedAdjacent =
    let updateMatch matchedAdjacent (prev : int[]) curr part =
        let last = if Array.isEmpty (prev) then None else Some (prev.[prev.Length - 1])
        match matchedAdjacent with
        | LockedInMatch -> LockedInMatch
        | NoMatch ->
            if last = Some curr
            then
                match part with
                | Part1 -> LockedInMatch
                | Part2 -> CurrentlyAdjacent curr
            else NoMatch
        | CurrentlyAdjacent x ->
            if last <> Some curr
            then LockedInMatch
            else SeqTooLong x
        | SeqTooLong x ->
            if x = curr
            then SeqTooLong x
            else NoMatch


let rec calc (min : int list) max (prev : int[]) (matchedAdjacent : MatchedAdjacent) part =
    match (min, max) with
    | [], [] -> 0
    | x::[], m::[] ->
        if x > m
        then 0
        else
            [ x .. m ]
            |> List.sumBy (fun x ->
                match MatchedAdjacent.updateMatch matchedAdjacent prev x part with
                | NoMatch | SeqTooLong _ -> 0
                | CurrentlyAdjacent _ | LockedInMatch -> 1)

    | x::min, m::max ->
        if m < x then 0
        else
            [ x+1 .. m ]
            |> List.map (List.replicate (List.length min + 1))
            |> (fun xs -> (x::min)::xs)
            |> List.map (fun (head::xs) ->
                let matchedAdjacent = MatchedAdjacent.updateMatch matchedAdjacent prev head part
                if head = m // use the max value rather then generating our own
                then calc xs max (Array.append prev [|head|]) matchedAdjacent part
                else
                    let max = List.replicate (List.length max) 9
                    calc xs max (Array.append prev [|head|]) matchedAdjacent part)
            |> List.sum
    | _, _ -> failwith "Uneven inputs"

let sw = System.Diagnostics.Stopwatch.StartNew ()
let day1Part1Result = calc [1;3;6;7;7;7] [5;9;5;7;3;0] [||] NoMatch Part1
printfn $"Part1 result: {day1Part1Result}, took: {sw.ElapsedMilliseconds}ms"
// 1873

sw = System.Diagnostics.Stopwatch.StartNew ()
let day1Part2Result = calc [1;3;6;7;7;7] [5;9;5;7;3;0] [||] NoMatch Part2
printfn $"Part2 result: {day1Part2Result}, took: {sw.ElapsedMilliseconds}ms"
// 1264
