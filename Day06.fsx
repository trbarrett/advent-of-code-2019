#load "./Helper.fsx"
// Day 6 - Finding Orbit lengths
//
// Method -
// Part 1: Put the data in a map, and use a depth first search to count
//
// Part 2 - Djkstras graph search would work fine. But this is a tree so
//          we can go with a simpler option. We can find the steps from the base
//          to each node, then remove the common path to find the nodes between
//          the two


open Helper
open System

type OrbitMap = Map<string, seq<string>>

let orbitMap =
    Helper.readInput "day06.txt"
    |> Seq.map (String.split ')')
    |> Seq.map (fun xs -> xs.[0], xs.[1])
    |> Seq.groupByTuple

let rec orbitCount' (orbitMap : OrbitMap) name curr : int =
    match orbitMap |> Map.tryFind name with
    | None -> curr
    | Some orbits ->
        orbits
        |> Seq.sumBy (fun next -> orbitCount' orbitMap next (curr + 1))
        |> ((+) curr)

let depthFirstCount orbitMap =
    orbitCount' orbitMap "COM" 0

let rec findOrbitPath (orbitMap : OrbitMap) name path target =
    if name = target
    then Some path
    else
        match orbitMap |> Map.tryFind name with
        | None -> None
        | Some orbits ->
            orbits
            |> Seq.toList
            |> Seq.tryPick (fun next -> findOrbitPath orbitMap next (next::path) target)

let rec findTransferCount orbitMap a b=
    let aOrbits = findOrbitPath orbitMap "COM" ["COM"] a |> Option.get |> Set
    let bOrbits = findOrbitPath orbitMap "COM" ["COM"] b |> Option.get |> Set

    let differencesA = Set.difference aOrbits bOrbits
    let differencesB = Set.difference bOrbits aOrbits

    // -2, because we don't count the start and end items
    // +1, because we would have removed the common item
    // -1, because we don't count the starting item
    Set.count differencesA + Set.count differencesB - 2

// 621125
let sw = System.Diagnostics.Stopwatch.StartNew ()
let result1 = depthFirstCount orbitMap
printfn $"Part1 result: {result1} complete took: {sw.ElapsedMilliseconds}ms"

// 550
sw = System.Diagnostics.Stopwatch.StartNew ()
let result2 = findTransferCount orbitMap "YOU" "SAN"
printfn $"Part2 result: {result2} complete took: {sw.ElapsedMilliseconds}ms"
