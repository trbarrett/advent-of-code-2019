#load "./Helper.fsx"

// Day 7 - For five intcode computers hocked up in a row, determine the best
//         "phase setting" input to get the largest output.
//         With Part 2 we hook the computers up so they form a feedback loop
//
// Method : Each "intcode program" returns control back to the main program
//          after an IO event, alowing the main program to feed output from
//          one program to the next. Each program's state is stored when it
//          becomes suspended, allowing us to restart it whenever we need.
//
// Other solutions:
//         We could have used F# MailBoxProcessor to acutaly run the processors
//         in parallel and pass events that way. Similarly we could have used
//         another actor framework, or multiple threads with some type of
//         Pub/Sub to achive the same thing. Given we only run one program at a
//         time that would leave most threads waiting though.


open Helper
open System
open System.Collections.Generic

module IntCodeComputer =
    type ProgramMemory = string[]
    type InstructionPointer = int

    type InputStream = Queue<int>

    type RunningProgram =
        { Memory : ProgramMemory
          Pointer : InstructionPointer
          Input : InputStream }

    let initializeProgram programCode (inputData : int seq) =
        { Memory = Array.copy programCode
          Pointer = 0
          Input = Queue<int>(inputData) }

    let saveRunningProgram mem pointer input =
        { Memory = mem
          Pointer = pointer
          Input = input }

    type Event =
        | Terminate
        | SuspendUntilNextInput of RunningProgram
        | PerformOutput of int * RunningProgram

    type ProgramState =
        | Running of RunningProgram
        | SuspendedWaitingInput of RunningProgram
        | Terminated

    type Mode =
        | PositionMode // 0
        | ImmediateMode // 1

    type Opcode =
        | Add of Mode * Mode // 1
        | Mul of Mode * Mode // 2
        | Input // 3
        | Output of Mode // 4
        | JumpIfTrue of Mode * Mode // 5
        | JumpIfFalse of Mode * Mode // 6
        | LessThan of Mode * Mode // 7
        | Equals of Mode * Mode // 8
        | Exit // 99
        | Unknown of int

    module Opcode =

        let instrMemCount = function
            | Add _ -> 4 // itself, first parameter, second parameter, output
            | Mul _ -> 4
            | Input -> 2
            | Output _ -> 2
            | JumpIfTrue _ -> 3 // itself, comparison parameter, jump parameter
            | JumpIfFalse _ -> 3
            | LessThan _ -> 4
            | Equals _ -> 4
            | Exit -> 1
            | Unknown _ -> 1

        let parse (str : string) =
            let opCode, opCodeLength =
                if str.Length = 1 then str, 1
                else str.Substring(str.Length - 2), 2
            let getMode parameterNo =
                if parameterNo + opCodeLength > str.Length // Any missing modes are 0
                then PositionMode
                elif str.[str.Length - opCodeLength - parameterNo] = '0'
                then PositionMode
                else ImmediateMode // '1'
            match int opCode with
            | 1 -> Add (getMode 1, getMode 2)
            | 2 -> Mul (getMode 1, getMode 2)
            | 3 -> Input
            | 4 -> Output (getMode 1)
            | 5 -> JumpIfTrue (getMode 1, getMode 2)
            | 6 -> JumpIfFalse (getMode 1, getMode 2)
            | 7 -> LessThan (getMode 1, getMode 2)
            | 8 -> Equals (getMode 1, getMode 2)
            | 99 -> Exit
            | x -> Unknown x

    let rec execute (input : InputStream) (mem : string[]) (pos : int) =
       let paramValue paramNo paramMode =
           if paramMode = PositionMode
           then int mem.[int mem.[pos + paramNo]]
           else int mem.[pos + paramNo]

       let opCode = Opcode.parse mem.[pos]
       match opCode with
       | Add (p1mode, p2mode) ->
           //printfn $"Pos: {pos} - Performing Add"
           let inAVal = paramValue 1 p1mode
           let inBVal = paramValue 2 p2mode
           let writeLoc = int mem.[pos + 3]
           mem.[writeLoc] <- string (inAVal + inBVal)
           //printfn $"Result: {mem.[writeLoc]} at pos: {writeLoc}"
           execute input mem (pos + (Opcode.instrMemCount opCode))

       | Mul (p1mode, p2mode) ->
           //printfn $"Pos: {pos} - Performing Mul"
           let inAVal = paramValue 1 p1mode
           let inBVal = paramValue 2 p2mode
           let writeLoc = int mem.[pos + 3]
           mem.[writeLoc] <- string (inAVal * inBVal)
           //printfn $"Result: {mem.[writeLoc]} at pos: {writeLoc}"
           execute input mem (pos + (Opcode.instrMemCount opCode))

       | Input ->
           //printfn $"Pos: {pos} - Performing Input"

           if input.Count > 0
           then
               let writeLoc = int mem.[pos + 1]
               mem.[writeLoc] <- string (input.Dequeue ())
               //printfn $"Result: {mem.[writeLoc]} at pos: {writeLoc}"
               execute input mem (pos + (Opcode.instrMemCount opCode))
           else
               SuspendUntilNextInput (saveRunningProgram  mem pos input)

       | Output pMode ->
           //printfn $"Pos: {pos} - Performing Output"
           let outVal = paramValue 1 pMode
           //printfn $"Output: {outVal}"
           let nextPos = (pos + (Opcode.instrMemCount opCode))
           PerformOutput (outVal, (saveRunningProgram  mem nextPos input))

       | JumpIfTrue (p1mode, p2mode)  ->
           //printfn $"Pos: {pos} - Performing Jump If True"
           let inAVal = paramValue 1 p1mode
           let nextPos =
               if inAVal <> 0
               then paramValue 2 p2mode
               else (pos + (Opcode.instrMemCount opCode))
           execute input mem nextPos

       | JumpIfFalse (p1mode, p2mode)  ->
           //printfn $"Pos: {pos} - Performing Jump If False"
           let inAVal = paramValue 1 p1mode
           let nextPos =
               if inAVal = 0
               then paramValue 2 p2mode
               else (pos + (Opcode.instrMemCount opCode))
           execute input mem nextPos

       | LessThan (p1mode, p2mode) ->
           let inAVal = paramValue 1 p1mode
           let inBVal = paramValue 2 p2mode
           let writeLoc = int mem.[pos + 3]
           mem.[writeLoc] <- if inAVal < inBVal then "1" else "0"
           execute input mem (pos + (Opcode.instrMemCount opCode))

       | Equals (p1mode, p2mode) ->
           let inAVal = paramValue 1 p1mode
           let inBVal = paramValue 2 p2mode
           let writeLoc = int mem.[pos + 3]
           mem.[writeLoc] <- if inAVal = inBVal then "1" else "0"
           execute input mem (pos + (Opcode.instrMemCount opCode))

       | Exit ->
           //printfn $"Pos: {pos} - Exit"
           Terminate

       | Unknown x ->
           failwithf $"Unknown opcode {x} at position {pos}"

    let runProgramFromState prog input =
       match prog with
       | Running p ->
           match input with
           | Some input -> p.Input.Enqueue input
           | None -> ()

           execute p.Input p.Memory p.Pointer
       | SuspendedWaitingInput p ->
           match input with
           | Some input -> p.Input.Enqueue input
           | None -> failwith "No input for program expecting input"

           execute p.Input p.Memory p.Pointer
       | Terminated ->
           Terminate


open IntCodeComputer

let prepareSeries program phaseSettings =
    phaseSettings
    |> List.map (fun phaseSetting ->
        initializeProgram program [phaseSetting]
        |> SuspendedWaitingInput)

let rec runSeries activePrograms input =
    match activePrograms with
    | [] -> input // return the final input
    | prog::rest ->
        let event = runProgramFromState prog input
        match event with
        | Terminate ->
            // go to the next program in the series
            runSeries rest input
        | SuspendUntilNextInput _ ->
            runSeries rest None
        | PerformOutput (output, _) ->
            // run the next program, using this outcome as the next
            // programs input since they are hooked up in series
            runSeries rest (Some output)

// The difference between this and `runSeries` is that after running a program
// that doesn't terminate, we add it to the back of the list of active programs
// so it can repeat
let rec runSeriesWithFeedback activePrograms input =
    match activePrograms with
    | [] -> input // return the final input
    | prog::rest ->
        let event = runProgramFromState prog input
        match event with
        | Terminate ->
            // go to the next program in the series
            runSeriesWithFeedback rest input
        | SuspendUntilNextInput p ->
            // I don't think this is actually possible with our current problem
            // set. A program is expected to output something before consuming it's
            // next input
            runSeriesWithFeedback (rest@[SuspendedWaitingInput p]) None
        | PerformOutput (output, p) ->
            // run the next program, using this outcome as the next
            // programs input since they are hooked up in series
            runSeriesWithFeedback (rest@[Running p]) (Some output)

let runBasicAmpHookup program phaseSettings =
    let preparedPrograms = prepareSeries program phaseSettings
    runSeries preparedPrograms (Some 0)

let runFeedbackAmpHookup program phaseSettings =
    let preparedPrograms = prepareSeries program phaseSettings
    runSeriesWithFeedback preparedPrograms (Some 0)

let programCode =
    Helper.readUncommentedLines "day7.txt"
    |> Seq.item 0
    |> String.split ','

let bruteForceAllPhaseSettings program phaseOptions ampHookup =
    let possiblePhaseSettings = List.permutations (Set phaseOptions) 5
    possiblePhaseSettings // try every combination of phaseOption
    |> List.map (ampHookup program)
    |> List.zip possiblePhaseSettings
    |> List.choose (fun (phaseSetting, output) -> output |> Option.map (fun o -> phaseSetting, o))
    //|> List.map (tee (fun (xs, result) -> printfn "[%s] - %d" (String.Join(';', xs)) result ))
    |> List.maxBy snd

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let phaseSetting, total =
        bruteForceAllPhaseSettings programCode [0;1;2;3;4] runBasicAmpHookup
    let phaseSettingStr = sprintf "[%s]" (String.Join(';', phaseSetting))
    printfn $"Part1 result: {phaseSettingStr} - {total}, took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: [2;3;1;0;4] - 67023

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let phaseSetting, total =
        bruteForceAllPhaseSettings programCode [5;6;7;8;9] runFeedbackAmpHookup
    let phaseSettingStr = sprintf "[%s]" (String.Join(';', phaseSetting))
    printfn $"Part2 result: {phaseSettingStr} - {total}, took: {sw.ElapsedMilliseconds}ms"
    // Correct Answer: [5;Day72.fsx8;7;9;6] - 7818398

part1 ()
part2 ()
