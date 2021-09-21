#load "./Helper.fsx"

// Day 5 - Extending the Intcode computer to understand input, output,
//         immediate mode vs position mode, jumps and comparisons


open Helper
open System

let startingMem =
    Helper.readInput "day5.txt"
    |> Seq.item 0
    |> String.split ','

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

let rec calc (input : int list)  (mem : string[]) (pos : int) =
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
       calc input mem (pos + (Opcode.instrMemCount opCode))

   | Mul (p1mode, p2mode) ->
       //printfn $"Pos: {pos} - Performing Mul"
       let inAVal = paramValue 1 p1mode
       let inBVal = paramValue 2 p2mode
       let writeLoc = int mem.[pos + 3]
       mem.[writeLoc] <- string (inAVal * inBVal)
       //printfn $"Result: {mem.[writeLoc]} at pos: {writeLoc}"
       calc input mem (pos + (Opcode.instrMemCount opCode))

   | Input ->
       //printfn $"Pos: {pos} - Performing Input"
       let writeLoc = int mem.[pos + 1]
       mem.[writeLoc] <- string (List.head input)
       //printfn $"Result: {mem.[writeLoc]} at pos: {writeLoc}"
       calc (List.tail input) mem (pos + (Opcode.instrMemCount opCode))

   | Output pMode ->
       //printfn $"Pos: {pos} - Performing Output"
       let outVal = paramValue 1 pMode
       printfn $"Output: {outVal}"
       calc input mem (pos + (Opcode.instrMemCount opCode))

   | JumpIfTrue (p1mode, p2mode)  ->
       //printfn $"Pos: {pos} - Performing Jump If True"
       let inAVal = paramValue 1 p1mode
       let nextPos =
           if inAVal <> 0
           then paramValue 2 p2mode
           else (pos + (Opcode.instrMemCount opCode))
       calc input mem nextPos

   | JumpIfFalse (p1mode, p2mode)  ->
       //printfn $"Pos: {pos} - Performing Jump If False"
       let inAVal = paramValue 1 p1mode
       let nextPos =
           if inAVal = 0
           then paramValue 2 p2mode
           else (pos + (Opcode.instrMemCount opCode))
       calc input mem nextPos

   | LessThan (p1mode, p2mode) ->
       let inAVal = paramValue 1 p1mode
       let inBVal = paramValue 2 p2mode
       let writeLoc = int mem.[pos + 3]
       mem.[writeLoc] <- if inAVal < inBVal then "1" else "0"
       calc input mem (pos + (Opcode.instrMemCount opCode))

   | Equals (p1mode, p2mode) ->
       let inAVal = paramValue 1 p1mode
       let inBVal = paramValue 2 p2mode
       let writeLoc = int mem.[pos + 3]
       mem.[writeLoc] <- if inAVal = inBVal then "1" else "0"
       calc input mem (pos + (Opcode.instrMemCount opCode))

   | Exit ->
       //printfn $"Pos: {pos} - Exit"
       mem

   | Unknown x ->
       failwithf $"Unknown opcode {x} at position {pos}"

let runDiagnostic1 mem =
    let mem = Array.copy mem
    calc [1] mem 0

let runDiagnostic5 mem =
    let mem = Array.copy mem
    calc [5] mem 0

let sw = System.Diagnostics.Stopwatch.StartNew ()
printfn $"Part 1: (Diagnostic 1)"
runDiagnostic1 startingMem |> ignore
printfn $"Part1 complete took: {sw.ElapsedMilliseconds}ms"

sw = System.Diagnostics.Stopwatch.StartNew ()
printfn $"Part 2: (Diagnostic 5)"
runDiagnostic5 startingMem |> ignore
printfn $"Part2 complete took: {sw.ElapsedMilliseconds}ms"
