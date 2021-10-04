#load "./Helper.fsx"

// Day 12 - Calculate how orbiting moons affect each others positions and
//          velocity via gravity
//
// Part 2 : There's a trick here, which is recognising each axis (z,y,z)
//          is completley independent of the others. From that we can calculate
//          how long each axis takes to cylce, and from there calcualte the
//          lowest common multiple.

open Helper
open System


type Moon = { Id: int; X: int; Y: int; Z: int }

type Moon1d = { Id: int; Pos: int }

type Vel1d =
    { Id: int; Vel: int }
    static member Zero = { Id = 0; Vel = 0; }

    static member ( + ) (x, y) =
        if x.Id = 0 then y elif y.Id = 0 then x
        else { Id = x.Id; Vel = x.Vel + y.Vel }

    static member print vel =
        sprintf $"vel={vel.Vel,2}"

module Moon1d =
    let updatePosition { Id = id; Vel = vel } { Id = id; Pos = pos } =
        {Id = id; Pos = pos + vel }

    let print moon1d =
        sprintf $"pos={moon1d.Pos,2}"

let gravityChange1d { Id = id1; Pos = pos1 } { Id = id2; Pos = pos2 } =
    let pos1Change = if pos1 = pos2 then 0 elif pos1 > pos2 then -1 else 1
    { Id = id1; Vel = pos1Change }, { Id = id2; Vel = -pos1Change }

let simulateMoonStep1d (moons1d, velocities1d) =
    let velocityChanges =
        moons1d
        |> List.combinations 2
        |> List.map (fun [a; b] -> gravityChange1d a b)
        |> List.collect (fun (a,b) -> [a; b])

    let velocities =
        velocityChanges @ velocities1d
        |> List.groupBy (fun x -> x.Id)
        |> List.map (snd >> List.sum)
        |> List.sortBy (fun x -> x.Id)

    List.zip moons1d velocities
    |> List.map (fun (moon, vel) -> Moon1d.updatePosition vel moon),
    velocities

let rec findCycleLengthFor1d' state acc count =
    if Set.contains state acc then count
    else
        let acc = Set.add state acc
        findCycleLengthFor1d' (simulateMoonStep1d state) acc (count + 1)

let rec gcd x y =
    if y = 0L then x
    else gcd y (x % y)

let lcm a b = a*b/(gcd a b)

let findAxisCycleLengths startingMoons  =
    let startingVelocities =
        [1..4] |> List.map (fun id -> { Vel1d.Zero with Id = id })

    let moonXToMoon1d { Id = id; X = x } = { Id = id; Pos = x }
    let moonYToMoon1d { Id = id; Y = y } = { Id = id; Pos = y }
    let moonZToMoon1d { Id = id; Z = z } = { Id = id; Pos = z }

    let startingMoonsX = startingMoons |> List.map moonXToMoon1d
    let startingMoonsY = startingMoons |> List.map moonYToMoon1d
    let startingMoonsZ = startingMoons |> List.map moonZToMoon1d

    // running these in parallel would cut the time by 1/3
    let xCycleLength = findCycleLengthFor1d' (startingMoonsX, startingVelocities) Set.empty 0 |> int64
    let yCycleLength = findCycleLengthFor1d' (startingMoonsY, startingVelocities) Set.empty 0 |> int64
    let zCycleLength = findCycleLengthFor1d' (startingMoonsZ, startingVelocities) Set.empty 0 |> int64

    printfn $"Cycle times (X, Y, Z): {(xCycleLength, yCycleLength, zCycleLength)}"

    lcm xCycleLength yCycleLength |> lcm zCycleLength

let startingPositions =
    [ { Id = 1; X =   1; Y =   4; Z =  4 }
      { Id = 2; X =  -4; Y =  -1; Z = 19 }
      { Id = 3; X = -15; Y = -14; Z = 12 }
      { Id = 4; X = -17; Y =   1; Z = 10 } ]

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let result = findAxisCycleLengths startingPositions
    printfn $"Part2 result: {result}, took: {sw.ElapsedMilliseconds}ms"
    // Part2 result: 583523031727256, took: 7620ms

part2 ()
