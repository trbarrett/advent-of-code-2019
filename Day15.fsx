#load "./Helper.fsx"
#load "./IntCodeComputer.fsx"

// Day 15 - Use an IntProgram to control a repair droid, and find the oxygen
//          then see how fast oxygen fills the space
//
// Part 1 - Use a breadth first search to explore the map until oxygen is
//          reached. We keep a copy of the game state for every frontier we
//          explore, which means making a copy every move. Makes things slow
//
// Part 2 - Used another breadth first traversalto flood-fill the map with
//          oxygen. The only slightly unusual thing about this is we filled the
//          entire frontier at each recursion. Normally you only do one step at
//          a time.

open System
open Helper
open IntCodeComputer

type Tiles = Map<int*int, char>

type Direction = | North | South | East | West
module Direction =
    let toIntCodeInputValue = function
        | North -> 1L | South -> 2L | West -> 3L | East -> 4L

type DroidStatus = | MoveBlocked | Moved | MovedToOxygen
module DroidStatus =
    let fromIntCodeOutput = function
        | 0L -> MoveBlocked // hit a wall, position unchanged
        | 1L -> Moved // moved one step in direction, empty space
        | 2L -> MovedToOxygen // moved one step in direction and found Oxygen

let getPositionInDirection dir droid =
    let x, y = droid
    match dir with
    | North -> (x, y - 1) | South -> (x, y + 1)
    | West -> (x - 1, y)  | East -> (x + 1, y)

let updateMap direction moveStatus tiles droid =
    let newPos = getPositionInDirection direction droid
    match moveStatus with
    | MoveBlocked -> Map.add newPos '█' tiles, droid
    | Moved -> Map.add newPos ' ' tiles, newPos
    | MovedToOxygen -> Map.add newPos '⬤' tiles, newPos

let ensureSuspendedWaitingInput pState =
    match pState with
    | Terminated -> failwith "Program terminated unexpectedly"
    | TimedOut -> failwith "Program execution timed out"
    | Running _ -> failwith "Program stuck in Running State"
    | SuspendedWaitingInput p -> p

let generateNewFrontier p tiles droid steps =
    [ North; South; West; East ]
    |> List.choose (fun dir ->
        let pos = getPositionInDirection dir droid
        match Map.containsKey pos tiles with
        | false -> Some dir
        | true -> None)
    |> List.map (fun dir ->
        (fun _ -> RunningProgram.makeCopy p), droid, dir, (dir::steps))

type Objective = | ExploreMap | DiscoverOxygen
type ObjectiveResult =
    | ExploredMap of Tiles
    | DiscoveredOxygen of Direction list

let rec breadthFirstSearch tiles frontier obj =
    match frontier with
    | [] ->
        match obj with
        | DiscoverOxygen -> failwith "Entire map searched, and no oxygen was found"
        | ExploreMap -> ExploredMap tiles
    | (p, droid, dir, steps)::frontier ->
        //printfn $"Frontier item: Droid at {droid} moving {dir}"
        let p = p ()
        p.Input.Enqueue(dir |> Direction.toIntCodeInputValue)
        let outputs, pState = executeUntilHalt p
        let p = ensureSuspendedWaitingInput pState
        let status = DroidStatus.fromIntCodeOutput outputs.[0]
        let tiles, droid = updateMap dir status tiles droid
        match status with
        | MoveBlocked ->
            //printfn $"Droid at {droid} blocked from moving {dir}"
            breadthFirstSearch tiles frontier obj
        | MovedToOxygen ->
            match obj with
            | ExploreMap ->
                let frontier = frontier @ (generateNewFrontier p tiles droid steps)
                breadthFirstSearch tiles frontier obj
            | DiscoverOxygen -> DiscoveredOxygen steps
        | Moved ->
            //printfn $"Moved droid at {droid} {dir}"
            let frontier = frontier @ (generateNewFrontier p tiles droid steps)
            breadthFirstSearch tiles frontier obj

let drawMap tiles droid =
    let mapSeq = tiles |> Map.toSeq

    let xs = mapSeq |> Seq.map (fun ((x,_), _) -> x)
    let ys = mapSeq |> Seq.map (fun ((_,y), _) -> y)

    let minX, maxX, minY, maxY =
        if Seq.isEmpty mapSeq
        then -1, 1, -1, 1
        else
            let minX, maxX = min (Seq.min xs) -1, max (Seq.max xs) 1
            let minY, maxY = min (Seq.min ys) -1, max (Seq.max ys) 1
            minX, maxX, minY, maxY

    printfn ""
    [minY .. maxY]
    |> List.iter (fun y ->
        printfn ""
        [minX .. maxX]
        |> List.iter (fun x ->
            if droid = (x,y)
            then printf "@"
            else
                let tile = Map.tryFind (x,y) tiles |> Option.defaultValue ' '
                printf "%c" tile))

let rec playLoop program tiles droid (steps : Direction list) =
    drawMap tiles droid

    let directionInput =
        match (Console.ReadKey(false).KeyChar) with
        | 'w' -> North | 's' -> South | 'a' -> West | 'd' -> East
        | _ -> failwith "Incorrect input. Needs to be w, a, s or d"

    program.Input.Enqueue(directionInput |> Direction.toIntCodeInputValue)
    let outputs, pState = executeUntilHalt program
    let droidStatus = DroidStatus.fromIntCodeOutput (List.head outputs)
    let tiles, droid = updateMap directionInput droidStatus tiles droid

    let p = ensureSuspendedWaitingInput pState
    playLoop p tiles droid (directionInput::steps)

let oxygenFrontier tiles pos =
    [ North; South; West; East ]
    |> List.choose (fun dir ->
        let pos = getPositionInDirection dir pos
        match Map.find pos tiles with
        | ' ' -> Some dir
        | _ -> None)
    |> List.map (fun dir -> pos, dir)

let rec fillWithOxygen tiles frontier count =
    let folder (tiles, next) (pos, dir) =
        let tiles, newPos = updateMap dir MovedToOxygen tiles pos
        tiles, (Set.add newPos next)

    let tiles, newPositions = List.fold folder (tiles, Set.empty) frontier

    // Uncomment the following lines if you want to watch the map get filled
    //drawMap tiles (0,0)
    //System.Threading.Thread.Sleep 50

    if Set.isEmpty newPositions
    then count
    else
       let newFrontier =
           newPositions
           |> Set.map (oxygenFrontier tiles)
           |> Set.toList |> List.collect id
       fillWithOxygen tiles newFrontier (count + 1)

let programCode =
    Helper.readLinesWithHashComments "day15.txt"
    |> Seq.item 0
    |> String.split ','

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    // uncomment the next line if you want to explore the map interactively
    //playLoop (initializeProgram programCode []) Map.empty (0,0) [] |> ignore
    let frontier = generateNewFrontier (initializeProgram programCode []) Map.empty (0,0) []
    let (DiscoveredOxygen shortestPath) = breadthFirstSearch Map.empty frontier DiscoverOxygen
    printfn $"\nPart1 result: {List.length shortestPath} took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: 308 took: 73ms

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let frontier = generateNewFrontier (initializeProgram programCode []) Map.empty (0,0) []
    let (ExploredMap tiles) = breadthFirstSearch Map.empty frontier ExploreMap
    let oxygenPos = Map.findKey (fun _ tile -> tile = '⬤') tiles
    let fillSteps = fillWithOxygen tiles (oxygenFrontier tiles oxygenPos) 0
    printfn $"\nPart2 result: {fillSteps} took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: 328 took: 95ms

part1()
part2()
