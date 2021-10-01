#load "./Helper.fsx"

// Day 11 - Use an IntProgram to paint a spaceship

open Helper
open Helper
open System
open System.Collections.Generic

module IntCodeComputer =
    type ProgramMemory = string[]
    type InstructionPointer = int

    type InputStream = Queue<int64>

    type RunningProgram =
        { Mem : ProgramMemory
          Pos : InstructionPointer
          RelBase : InstructionPointer
          Input : InputStream }

    let initializeProgram (programCode : string []) (inputData : int64 seq) =
        let mem = Array.create 10_000 "0"
        programCode.CopyTo(mem, 0L)
        { Mem = mem
          Pos = 0
          RelBase = 0
          Input = Queue<int64>(inputData) }

    type Event =
        | Terminate
        | SuspendUntilNextInput of RunningProgram
        | PerformOutput of int64 * RunningProgram

    type ProgramState =
        | Running of RunningProgram
        | SuspendedWaitingInput of RunningProgram
        | Terminated

    type Mode =
        | PositionMode // 0
        | ImmediateMode // 1
        | RelativeMode // 2

    type Opcode =
        | Add of Mode * Mode * Mode // 1
        | Mul of Mode * Mode * Mode // 2
        | Input of Mode  // 3
        | Output of Mode // 4
        | JumpIfTrue of Mode * Mode // 5
        | JumpIfFalse of Mode * Mode // 6
        | LessThan of Mode * Mode * Mode // 7
        | Equals of Mode * Mode * Mode // 8
        | AdjustRelativeBase of Mode // 9
        | Exit // 99
        | Unknown of int

    module Opcode =

        let instrMemCount = function
            | Add _ -> 4 // itself, first parameter, second parameter, output
            | Mul _ -> 4
            | Input _ -> 2
            | Output _ -> 2
            | JumpIfTrue _ -> 3 // itself, comparison parameter, jump parameter
            | JumpIfFalse _ -> 3
            | LessThan _ -> 4
            | Equals _ -> 4
            | AdjustRelativeBase _ -> 2
            | Exit -> 1
            | Unknown _ -> 1

        let parse (str : string) =
            let opCode, opCodeLength =
                if str.Length = 1 then str, 1
                else str.Substring(str.Length - 2), 2
            let getMode parameterNo =
                if parameterNo + opCodeLength > str.Length // Any missing modes are 0
                then PositionMode
                else
                    match str.[str.Length - opCodeLength - parameterNo] with
                    | '0' -> PositionMode
                    | '1' -> ImmediateMode
                    | '2' -> RelativeMode
                    | modeCh -> failwithf "Unexpected parameter mode: '%c'" modeCh

            match int opCode with
            | 1 -> Add (getMode 1, getMode 2, getMode 3)
            | 2 -> Mul (getMode 1, getMode 2, getMode 3)
            | 3 -> Input (getMode 1)
            | 4 -> Output (getMode 1)
            | 5 -> JumpIfTrue (getMode 1, getMode 2)
            | 6 -> JumpIfFalse (getMode 1, getMode 2)
            | 7 -> LessThan (getMode 1, getMode 2, getMode 3)
            | 8 -> Equals (getMode 1, getMode 2, getMode 3)
            | 9 -> AdjustRelativeBase (getMode 1)
            | 99 -> Exit
            | x -> Unknown x

        let getParameterMode opCode paramNo =
            match opCode with
            | Add (a, b, c) | Mul (a, b, c)
            | LessThan (a, b, c) | Equals (a, b, c) ->
                match paramNo with
                | 1 -> a | 2 -> b | 3 -> c
                | _ -> failwith $"Opcode [%A{opCode}] does not have param no: {paramNo}"
            | JumpIfTrue (a, b) | JumpIfFalse (a, b) ->
                match paramNo with
                | 1 -> a | 2 -> b
                | _ -> failwith $"Opcode [%A{opCode}] does not have param no: {paramNo}"
            | Input a | Output a
            | AdjustRelativeBase a ->
                match paramNo with
                | 1 -> a
                | _ -> failwith $"Opcode [%A{opCode}]does not have param no: {paramNo}"
            | Exit
            | Unknown _ -> failwith $"Opcode [%A{opCode}] does not have any parameters"

        let fetchReadParamValue (p : RunningProgram) opCode paramNo =
            let valueAtParam = p.Mem.[p.Pos + paramNo]
            match getParameterMode opCode paramNo with
            | PositionMode -> int64 p.Mem.[int valueAtParam]
            | ImmediateMode ->
                //printfn $"Getting param {paramNo} value via immediate mode. Value: {int64 valueAtParam}"
                int64 valueAtParam
            | RelativeMode ->
                int64 p.Mem.[p.RelBase + int valueAtParam]

        let fetchWriteParamValue (p : RunningProgram) opCode paramNo =
            let valueAtParam = p.Mem.[p.Pos + paramNo]
            match getParameterMode opCode paramNo with
            | PositionMode -> int valueAtParam
            | ImmediateMode -> failwith "Parameters that an instruction writes to will never be in immediate mode."
            | RelativeMode -> int p.RelBase + int valueAtParam


        let hasWriteParameter opCode =
            match opCode with
            | Add _ | Mul _ | LessThan _ | Equals _ -> true
            | Input _ -> false // input writes in special way
            | _ -> false

    // Debug helper to print instructions about the current opcode
    let printCurrInstructionDebug (p : RunningProgram) opCode =
        let arguments =
            [0..(Opcode.instrMemCount opCode - 1)]
            |> List.map (fun paramNo -> p.Mem.[p.Pos + paramNo])
            |> fun xs -> String.Join(",", xs)

        let evalArguments =
            [0..(Opcode.instrMemCount opCode - 1)]
            |> List.map (fun paramNo ->
                if paramNo = 0 || (Opcode.hasWriteParameter opCode && paramNo = Opcode.instrMemCount opCode - 1)
                then p.Mem.[p.Pos + paramNo]
                elif Opcode.hasWriteParameter opCode && paramNo = Opcode.instrMemCount opCode - 1
                then Opcode.fetchWriteParamValue p opCode paramNo |> string
                else Opcode.fetchReadParamValue p opCode paramNo |> string)
            |> fun xs -> String.Join(",", xs)
        printfn $"{p.Pos}: {arguments} - %A{opCode} (relbase:{p.RelBase})"
        printf $"{p.Pos}: {evalArguments}\n\t"

    let rec execute (p : RunningProgram) =
        // TODO - We could make this extract the actual values
        let opCode = Opcode.parse p.Mem.[p.Pos]
        //printCurrInstructionDebug p opCode
        match opCode with
        | Add _ ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let inBVal = Opcode.fetchReadParamValue p opCode 2
            let writeLoc = Opcode.fetchWriteParamValue p opCode 3
            //printfn $"Performing Add: {inAVal} + {inBVal} = {inAVal + inBVal} written to pos: {writeLoc}"
            p.Mem.[writeLoc] <- string (inAVal + inBVal)
            execute { p with Pos = p.Pos + (Opcode.instrMemCount opCode) }

        | Mul _ ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let inBVal = Opcode.fetchReadParamValue p opCode 2
            let writeLoc = Opcode.fetchWriteParamValue p opCode 3
            //printfn $"Performing Mul: {inAVal} * {inBVal} = {inAVal * inBVal} written to pos: {writeLoc}"
            p.Mem.[writeLoc] <- string (inAVal * inBVal)
            execute { p with Pos = p.Pos + (Opcode.instrMemCount opCode) }

        | Input _ ->
            if p.Input.Count > 0
            then
                let writeLoc = Opcode.fetchWriteParamValue p opCode 1
                p.Mem.[int writeLoc] <- string (p.Input.Dequeue ())
                //printfn $"Performing Input: Got {p.Mem.[int writeLoc]}, storing at pos: {writeLoc}"
                execute { p with Pos = p.Pos + (Opcode.instrMemCount opCode) }
            else
                SuspendUntilNextInput p

        | Output _ ->
            let outVal = Opcode.fetchReadParamValue p opCode 1
            //printfn $"Output: {outVal}"
            let nextPos = (p.Pos + (Opcode.instrMemCount opCode))
            PerformOutput (outVal, { p with Pos = nextPos })

        | JumpIfTrue _  ->
            //printfn $"Pos: {pos} - Performing Jump If True"
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let nextPos =
                if inAVal <> 0L
                then int (Opcode.fetchReadParamValue p opCode 2) // jump
                else p.Pos + (int (Opcode.instrMemCount opCode)) // step

            //printfn $"Performing JumpIfTrue. Jump to {Opcode.fetchReadParamValue p opCode 2} if {inAVal} = 1"
            execute { p with Pos = nextPos }

        | JumpIfFalse _ ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let nextPos =
                if inAVal = 0L
                then int (Opcode.fetchReadParamValue p opCode 2) // jump
                else (p.Pos + int (Opcode.instrMemCount opCode)) // step
            execute { p with Pos = nextPos }

        | LessThan _ ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let inBVal = Opcode.fetchReadParamValue p opCode 2
            let writeLoc = Opcode.fetchWriteParamValue p opCode 3
            p.Mem.[writeLoc] <- if inAVal < inBVal then "1" else "0"
            //printfn $"Performing LessThan: {inAVal} < {inBVal}: {p.Mem.[writeLoc]} written to pos: {writeLoc}"
            execute { p with Pos = p.Pos + (Opcode.instrMemCount opCode) }

        | Equals _ ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            let inBVal = Opcode.fetchReadParamValue p opCode 2
            let writeLoc = Opcode.fetchWriteParamValue p opCode 3
            p.Mem.[writeLoc] <- if inAVal = inBVal then "1" else "0"
            //printfn $"Performing Equals: {inAVal} = {inBVal}: {p.Mem.[writeLoc]} written to pos: {writeLoc}"
            execute { p with Pos = p.Pos + (Opcode.instrMemCount opCode) }

        | AdjustRelativeBase p1mode ->
            let inAVal = Opcode.fetchReadParamValue p opCode 1
            //printfn $"Pos: {p.Pos} - Adjusting Relative base from {p.RelBase} by {inAVal} to {p.RelBase + int inAVal}"
            execute { p with
                        Pos = p.Pos + (Opcode.instrMemCount opCode)
                        RelBase = p.RelBase + int inAVal }

        | Exit ->
            //printfn $"Pos: {pos} - Exit"
            Terminate

        | Unknown x ->
            failwithf $"Unknown opcode {x} at position {p.Pos}"

    let rec executeUntilHalt (p : RunningProgram) =
        let rec executeUntilHalt' (p : RunningProgram) acc =
            match execute p with
            | Terminate -> acc, Terminated
            | SuspendUntilNextInput p -> acc, SuspendedWaitingInput p
            | PerformOutput (o, p) -> executeUntilHalt' p (o::acc)
        let output, p = executeUntilHalt' p []
        List.rev output, p

open IntCodeComputer


type Turn = | Left90Deg | Right90Deg
module Turn =
    let fromInt64 = function | 0L -> Left90Deg | 1L -> Right90Deg

type Facing = | Up | Right | Left | Down
module Facing =
    let turn facing turn =
        match facing, turn with
        | Up, Right90Deg -> Right
        | Right, Right90Deg -> Down
        | Down, Right90Deg -> Left
        | Left, Right90Deg -> Up
        | Up, Left90Deg -> Left
        | Left, Left90Deg -> Down
        | Down, Left90Deg -> Right
        | Right, Left90Deg -> Up

    let stepForward (x,y) = function
        | Up ->    (x,     y - 1)
        | Down ->  (x,     y + 1)
        | Right -> (x + 1, y)
        | Left ->  (x - 1, y)

type PaintColour = | Black | White
module PaintColour =
    let toInt64 = function | Black -> 0L | White -> 1L
    let fromInt64 = function | 0L -> Black | 1L -> White

let rec paintHull (p : RunningProgram) (hull : Map<int*int, PaintColour>) pos facing =
    let panelColour =
        Map.tryFind pos hull
        |> Option.defaultValue (Black)

    p.Input.Enqueue (PaintColour.toInt64 panelColour)
    let outputs, pState = executeUntilHalt p

    let hull, newPos, newFacing =
        match List.length outputs with
        | 0 -> hull, pos, facing
        | 2 ->
            let newColour = PaintColour.fromInt64 outputs.[0]
            let turn = Turn.fromInt64 outputs.[1]

            // paint the hull
            let hull = Map.add pos newColour hull

            // turn and move the robot forward
            let facing = Facing.turn facing turn
            let pos = Facing.stepForward pos facing
            hull, pos, facing
        | _ -> failwith "Unexpected output count"

    match pState with
    | Terminated -> hull
    | Running _ -> failwith $"Program Stuck at `Running`"
    | SuspendedWaitingInput p -> paintHull p hull newPos newFacing

let paintShip programCode startingHull =
    let p = initializeProgram programCode []
    paintHull p startingHull (0,0) Up

let printHull (hull : Map<int*int, PaintColour>) =
    let hullSeq = hull |> Map.toSeq

    let xs = hullSeq |> Seq.map (fun ((x,_), _) -> x)
    let ys = hullSeq |> Seq.map (fun ((_,y), _) -> y)

    let minX, maxX = Seq.min xs, Seq.max xs
    let minY, maxY = Seq.min ys, Seq.max ys

    [minY .. maxY]
    |> List.iter (fun y ->
        printfn ""
        [minX .. maxX]
        |> List.iter (fun x ->
            let colour = Map.tryFind (x,y) hull |> Option.defaultValue Black
            match colour with
            | Black -> printf " "
            | White -> printf "#"))

let programCode =
    Helper.readLinesWithHashComments "day11.txt"
    |> Seq.item 0
    |> String.split ','

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let hull = paintShip programCode Map.empty
    printfn $"Part1 result: {Map.count hull}, took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: 2441, took: 84ms

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let hull = paintShip programCode (Map [(0,0), White])
    printfn $"Part2 result image below:"
    printHull hull
    printfn $"\n\nPart2 took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: PZRFPRKC, took: 7ms

part1 ()
part2 ()
