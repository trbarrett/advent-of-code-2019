#load "./Helper.fsx"

// Day 10 - Find asteroids visible from other asteroids, taking into account
//          ones that are blocked by line of sight
//
// Method:
//   Part1 -
//         For each asteroid, work outwards in rings adding the asteroid's
//         relative position to a dictionary, but with a simplified/normalized
//         fraction as the key. i.e. instead of x: 6, y: 2 (6,2) you would
//         simplify the fraction to (3,1). Any duplicated simplified fraction
//         is not visible.
//
//   Part2 -
//         For part2 we need to remove the asteroids in order, taking into
//         account only what's visible. My solution here was to sort the
//         asteroids by their relative position, converted into radians
//         (via arctan). If there was less than 200 asteroids visible just
//         remove them all and iterate


open Helper
open System

type Contents = | Empty | Asteroid

let rec gcd x y =
    if y = 0 then x
    else gcd y (x % y)

let asteroidLocations asteroidMap =
    asteroidMap
    |> Seq.indexed
    |> Seq.collect (fun (y, xs) ->
        xs
        |> Seq.indexed
        |> Seq.choose (fun (x, sector) ->
            match sector with
            | Empty -> None
            | Asteroid -> Some (x,y)))
    |> Set

// find all the positions along a ring around a center position
let getRingPositions (width, height) ((x,y) as centre) (ringNo : int) =
    let xRange = [(x - ringNo) .. (x + ringNo)]
    let yRange = [(y - ringNo) .. (y + ringNo)]

    let top = xRange |> List.map (fun x -> (x, y - ringNo))
    let bottom = xRange |> List.map (fun x -> (x, y + ringNo))
    let left = yRange |> List.map (fun y -> (x - ringNo, y))
    let right = yRange |> List.map (fun y -> (x + ringNo, y))

    // only include the positions within the map
    top @ bottom @ left @ right
    |> Set |> Set.filter (fun (x,y) ->
        x >= 0 && y >= 0 && x < width && y < height)

let relativePosition (xCentre, yCenter) ((x, y) : int * int) =
    x - xCentre, y - yCenter

let mapDimensions asteroidMap =
    let height = Seq.length asteroidMap
    let width = asteroidMap |> Seq.item 0 |> Seq.length
    (width, height)

let visibleInRing ringAsteroids ((x, y) as asteroidLocation) (visible : Map<int * int, int * int>) =
    (visible, ringAsteroids)
    ||> Set.fold (fun visible (asteroidX, asteroidY) ->
        let offsetX, offsetY = relativePosition (x,y) (asteroidX, asteroidY)
        let offsetGCD =
            // can't find a GCD of 0. But if they are on an axis we'll
            // normalize to 1
            match Math.Min (Math.Abs offsetX, Math.Abs offsetY) with
            | 0 -> Math.Max (Math.Abs offsetX, Math.Abs offsetY)
            | _ -> gcd (Math.Abs offsetX) (Math.Abs offsetY)
        let baseX, baseY = offsetX/offsetGCD, offsetY/offsetGCD
        match Map.tryFind (baseX, baseY) visible with
        | Some _ ->
            visible // view must be blocked by an existing asteroid
        | None ->
            Map.add (baseX, baseY) (asteroidX, asteroidY) visible)

let findVisibleFromAsteroid ((mapWidth, mapHeight) : int * int) allAsteroids asteroidLocation =
    // work increasingly outwards in rings to find asteroids, seeing
    // if they would be blocked by items in the inner rings
    let maxDimension : int = (Math.Max(mapWidth, mapHeight))
    let ringAsteroids =
        [1..maxDimension]
        |> List.map (getRingPositions (mapWidth, mapHeight) asteroidLocation)
        |> List.map (Set.intersect allAsteroids)
    (Map.empty, ringAsteroids)
    ||> List.fold (fun visible ring -> visibleInRing ring asteroidLocation visible)

let getMapVisibility asteroidMap =
    let dimensions = mapDimensions asteroidMap
    let asteroids = asteroidLocations asteroidMap
    let asteroidsList = List.ofSeq asteroids
    asteroidsList
    |> List.map (findVisibleFromAsteroid dimensions asteroids)
    |> List.zip asteroidsList

let getAsteroidWithMostVisible asteroidMap =
    getMapVisibility asteroidMap
    |> List.maxBy (snd >> Map.count)
    |> fun (pos, map) -> (pos, Map.count map)

let get200thVaporizedAsteroid asteroidMap asteroidLocation =
    let dimensions = mapDimensions asteroidMap
    let asteroids = asteroidLocations asteroidMap
    let asteroids = Set.difference asteroids (Set [asteroidLocation])
    let rec vaporizeVisible remainingAsteroids count =
        let vaporized =
            findVisibleFromAsteroid dimensions remainingAsteroids asteroidLocation

        if (Map.count vaporized + count < 200)
        then
            vaporized
            |> Map.toList |> List.unzip |> snd |> Set
            |> Set.difference remainingAsteroids
            |> fun nowRemaining ->
                vaporizeVisible nowRemaining (count + Map.count vaporized)
        else
           let vaporizedOrder =
               vaporized
                |> Map.toList
                |> List.sortBy (fun ((x, y), _) ->
                    let rad = Math.Atan2(float y, float x)
                    // Atan2 results start from the x axis on the negative side
                    // going clockwise (given our y's are flipped from a
                    // standard math coordinate system). We need to start from
                    // the vertical position, so move ones in the topleft
                    // quadrant around by 1 revolution to put them last
                    if rad < (-Math.PI/2.)
                    then rad + Math.PI * 2.
                    else rad)

           // find the 200th item vaporized
           vaporizedOrder
           |> List.item (200 - count - 1) // -1 because we're counting from 0

    vaporizeVisible asteroids 0

let asteroidMap =
    Helper.readLinesWithSlashComments "day10.txt"
    |> Seq.map (Seq.map (fun x -> if x = '.' then Empty else Asteroid))

let sw = System.Diagnostics.Stopwatch.StartNew ()
let part1Result = getAsteroidWithMostVisible asteroidMap
printfn $"Part1 result, asteroid with most visible: %A{part1Result}, took: {sw.ElapsedMilliseconds}ms"
// Correct answer: asteroid with most visible: (22, 25), with 286 others visible

sw = System.Diagnostics.Stopwatch.StartNew ()
let asteroidWithMostVisibility = fst part1Result
let part2Result = get200thVaporizedAsteroid asteroidMap asteroidWithMostVisibility
printfn $"Part2 result, 200th vaporized: %A{part2Result |> snd}, took: {sw.ElapsedMilliseconds}ms"
// Correct answer: 200th vaporized: (5, 4) - Answer is 504
