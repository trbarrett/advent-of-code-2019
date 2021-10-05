#load "./Helper.fsx"
#load "./IntCodeComputer.fsx"

// Day 13 - Use an IntProgram to run an arcade cabinet
//
// Part 1 - This was a straightforwards bit of code, just need to pass the keep
//          a copy of the score around
//
// Part 2 - Play a brakeout game! To solve it you can actually play the game, or
//          try a different method.
//
// Method: I did a depth first search of the search space, with a hueristic to
//         keep the paddle close to the ball. Unfortunatley I ran into an issue
//         with the actual intcode where it gets stuck in an infinite loop. Tohh
//         Here's a description on reddit:
//         https://www.reddit.com/r/adventofcode/comments/ef5n9e/2019_day_13_found_a_bug_in_the_intcode_breakout/
//         To get around that I added a timeout on the execution of the intcode
//         computer, but it certainly made things a lot slower to have to raise
//         and catch lots of timeout exceptions.

open System
open Helper
open IntCodeComputer

type Screen =
    { Tiles : Map<int*int, char>
      Score : int64
      BallPos : (int*int) option
      PaddlePos : (int*int) option}

module Screen =
    let empty = { Tiles = Map.empty; Score = 0L; BallPos = None; PaddlePos = None }

let drawTileToScreen screen tileInstructions =
    match tileInstructions with
    | [-1L; 0L; score] -> { screen with Score = score }
    | [x; y; tileId] ->
        let tile =
            match tileId with
            | 0L -> ' ' // empty tile
            | 1L -> '█' // wall tile
            | 2L -> '▒' // block tile
            | 3L -> '▂' // horizontal paddle
            | 4L -> '⬤' // ball

        { screen with
            Tiles = Map.add (int x, int y) tile screen.Tiles
            PaddlePos = if tile = '▂' then Some (int x, int y) else screen.PaddlePos
            BallPos = if tile = '⬤' then Some (int x, int y) else screen.BallPos }
    | [] -> screen

let rec gatherTiles (p : RunningProgram) (screen : Screen) =
    let outputs, pState = executeUntilHalt p
    outputs
    |> List.chunkBySize 3
    |> List.fold drawTileToScreen screen

let drawScreen (screen : Screen) =
    let screenSeq = screen.Tiles |> Map.toSeq

    let xs = screenSeq |> Seq.map (fun ((x,_), _) -> x)
    let ys = screenSeq |> Seq.map (fun ((_,y), _) -> y)

    let minX, maxX = 0, Seq.max xs
    let minY, maxY = 0, Seq.max ys

    // Note: screen size: (0-36, 0-21)
    printfn $"\nScore: {screen.Score}"

    [minY .. maxY]
    |> List.iter (fun y ->
        printfn ""
        [minX .. maxX]
        |> List.iter (fun x ->
            let tile = Map.tryFind (x,y) screen.Tiles |> Option.defaultValue ' '
            printf "%c" tile))

let countBlockTiles screen =
    screen.Tiles
    |> Map.filter (fun _ tile -> tile = '▒')
    |> Map.count

type IterationResult =
    | RemainingWallTiles of int
    | FinalScore of int64
    | Error of string

let paddleDirectionOptions screen =
    let options = [-1L; 1L; 0L]
    match (screen.BallPos, screen.PaddlePos) with
    | None, None | _, None | None, _ -> options
    | Some (ballX, _), Some (paddleX, _) ->
        // can't move left when at left edge
        let options =
            if paddleX = 1
            then options |> List.filter (fun x -> x <> -1L)
            else options

        // can't move right when at right edge
        let options =
            if paddleX = 35
            then options |> List.filter (fun x -> x <> 1L)
            else options

        // if the paddle is 2 left of the ball, then no point moving further left
        let options =
            if paddleX - ballX < -1
            then [1L] //options |> List.filter (fun x -> x <> -1L)
            else options

        // if the paddle is 2 right of the ball, then can't moving further right
        let options =
            if paddleX - ballX > 1
            then [-1L] //options |> List.filter (fun x -> x <> 1L)
            else options

        options

// example of a case where the intcode gets stuck in an infinite loop
let broken = [0;0;0;0;0;0;0;0;0;-1;-1;-1;-1;-1;-1;1;1;1;1;1;1;-1;-1;-1;-1;-1;-1;1;1;1;1;1;1;-1;-1;-1;-1;-1;-1;1;1;1;1;1;-1;-1;-1;-1;-1;1;1;1;1;-1;-1;-1;-1;1;1;1;1;0;-1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;-1]
             |> List.map int64

let rec depthFirstSearchGameState program screen (steps : int64 list) : IterationResult option =
    let outputs, pState = executeUntilHaltOrTimeout program 20
    let screen =
        outputs
        |> List.chunkBySize 3
        |> List.fold drawTileToScreen screen

    match pState with
    | Terminated ->
        match countBlockTiles screen with
        | 0 -> Some (FinalScore screen.Score)
        | n ->
            //printfn $"FailBranch of {RemainingWallTiles n} after {List.length steps} moves"
            //if Seq.length steps > 20 then
            //    let moves = Seq.take 20 steps |> (fun xs -> String.Join(";", xs))
            //    printfn $"Last 20 moves: {moves}"
            Some (RemainingWallTiles n)
    | TimedOut ->
        let allMoves = steps |> (fun xs -> String.Join(";", xs))
        //printfn $"Timeout "
        Some (Error "Execution timed out")
    | Running _ -> failwith "Program stuck in Running State"
    | SuspendedWaitingInput p ->
        //if screen.Score <> oldScreen.Score
        //then printfn $"New score: {screen.Score} - remaining blocks: {countBlockTiles screen} after {List.length steps} moves"
        paddleDirectionOptions screen
        |> List.tryPick (fun input ->
            let p = RunningProgram.makeCopy p
            p.Input.Enqueue(input)
            match depthFirstSearchGameState p screen (input::steps) with
            | Some (FinalScore _) as r -> r
            | _ -> None)

let playUntilWin programCode input =
    let program = (initializeProgram programCode input)
    depthFirstSearchGameState program Screen.empty []

let rec playLoop program screen (steps : int64 list) =
    let outputs, pState = executeUntilHalt program
    let screen =
        outputs
        |> List.chunkBySize 3
        |> List.fold drawTileToScreen screen

    drawScreen screen

    match pState with
    | Terminated ->
        let allMoves = steps |> (fun xs -> String.Join(";", xs))
        printfn $"Trying with input: {allMoves}"
        screen
    | TimedOut ->
        let allMoves = steps |> (fun xs -> String.Join(";", xs))
        printfn $"Timeout for moves: {allMoves}"
        failwith "Execution timed out"

    | Running _ -> failwith "Program stuck in Running State"
    | SuspendedWaitingInput p ->
        let input = (Console.ReadKey(false).KeyChar)
        let joystickInput =
            match input with
            | 'a' -> -1L
            | 's' -> 0L
            | 'd' -> 1L
            | _ ->
                failwith "Incorrect input. Needs to be a, s or d"
        p.Input.Enqueue(joystickInput)
        playLoop p screen (joystickInput::steps)

let playGame programCode =
    let program = (initializeProgram programCode [])
    playLoop program Screen.empty

let programCodePart1 =
    Helper.readLinesWithHashComments "day13.txt"
    |> Seq.item 0
    |> String.split ','

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let screen = gatherTiles (initializeProgram programCodePart1 []) Screen.empty
    // drawScreen screen // uncommment this if you want to see the starting screen
    printfn $"\nPart1 result: {countBlockTiles screen} took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: 265 took: 30ms

let programCodePart2 =
    Helper.readLinesWithHashComments "day13part2.txt"
    |> Seq.item 0
    |> String.split ','

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    // use this code instead if you want to play the game with the keyboard
    //let result = (playGame programCodePart2 []).Score
    let result = playUntilWin programCodePart2 []
    printfn $"\nPart2 result: {result} took: {sw.ElapsedMilliseconds}ms"
    // Correct result: Completed with score 13331 after 6468 moves, took: 5391ms

part1()
part2()
