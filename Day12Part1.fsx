#load "./Helper.fsx"

// Day 12 - Calculate how orbiting moons affect each others positions and
//          velocity via gravity
//
// Part 1 : A bunch of basic calculations applied repeatedly, but pretty
//          straight forwards. I've used a monid to make summing easier

open Helper

type Moon = { Id: int; X: int; Y: int; Z: int }

type MoonVel =
    { Id: int; VX: int; VY: int; VZ: int }

    static member Zero = { Id = 0; VX = 0; VY = 0; VZ = 0; }

    static member ( + ) (x, y) =
        if x.Id = 0 then y elif y.Id = 0 then x
        else { Id = x.Id; VX = x.VX + y.VX; VY = x.VY + y.VY; VZ = x.VZ + y.VZ }

    static member kineticEnergy vel = abs vel.VX + abs vel.VY + abs vel.VZ

    static member print vel =
        sprintf $"vel=<x={vel.VX,2}, y={vel.VY,2}, z={vel.VZ,2}>"

module Moon =
    let updatePosition
        { Id = id; VX = dX; VY = dY; VZ = dZ }
        { Id = id; X = x; Y = y; Z = z }
        =
        { Id = id; X = x + dX; Y = y + dY; Z = z + dZ }

    let print moon = sprintf $"pos=<x={moon.X,2}, y={moon.Y,2}, z={moon.Z,2}>"

    let potentialEnergy moon = abs moon.X + abs moon.Y + abs moon.Z


let gravityChange
    { Id = id1; X = x1; Y = y1; Z = z1 }
    { Id = id2; X = x2; Y = y2; Z = z2 }
    =
    let axisChange a b = if a = b then 0 elif a > b then -1 else 1
    let dX = axisChange x1 x2
    let dY = axisChange y1 y2
    let dZ = axisChange z1 z2
    { Id = id1; VX = dX; VY = dY; VZ = dZ}, { Id = id2; VX = -dX; VY = -dY; VZ = -dZ}

let simulateMoonStep (moons, velocities) =
    let velocityChanges =
        moons
        |> List.combinations 2
        |> List.map (fun [a; b] -> gravityChange a b)
        |> List.collect (fun (a,b) -> [a; b])

    let velocities =
        velocityChanges @ velocities
        |> List.groupBy (fun x -> x.Id)
        |> List.map (snd >> List.sum)
        |> List.sortBy (fun x -> x.Id)

    List.zip moons velocities
    |> List.map (fun (moon, vel) -> Moon.updatePosition vel moon),
    velocities

let runSimulation startingMoons steps =
    let startingVelocities =
        [1..4] |> List.map (fun id -> { MoonVel.Zero with Id = id })

    (startingMoons, startingVelocities)
    |> (Seq.replicate steps simulateMoonStep |> Seq.reduce (>>))

let totalEnergy (moons, velocities) =
    List.zip moons velocities
    |> List.sumBy (fun (moon,vel) ->
        Moon.potentialEnergy moon * MoonVel.kineticEnergy vel)

let startingPositions =
    [ { Id = 1; X =   1; Y =   4; Z =  4 }
      { Id = 2; X =  -4; Y =  -1; Z = 19 }
      { Id = 3; X = -15; Y = -14; Z = 12 }
      { Id = 4; X = -17; Y =   1; Z = 10 } ]

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let result = runSimulation startingPositions 1000 |> totalEnergy
    printfn $"Part1 result: {result}, took: {sw.ElapsedMilliseconds}ms"
    // Part1 result: 10635, took: 16ms

part1 ()
