#load "./Helper.fsx"

open Helper

let startingMem =
    Helper.readInput "day02.txt"
    |> Seq.item 0
    |> String.split ','
    |> Array.map int

//printfn $"Starting mem : {startingMem}"

type Opcode =
    | Add // 1
    | Mul // 2
    | Exit // 99
    | Unknown of int

module Opcode =
    let parse = function
        | 1 -> Add
        | 2 -> Mul
        | 99 -> Exit
        | x -> Unknown x

let rec calc (mem : int[]) (pos : int) =
   match Opcode.parse mem.[pos] with
   | Add ->
       //printfn $"Pos: {pos} - Performing Add"
       let inAPos = mem.[pos + 1]
       let inBPos = mem.[pos + 2]
       let outLoc = mem.[pos + 3]
       mem.[outLoc] <- mem.[inAPos] + mem.[inBPos]
       //printfn $"Result: {mem.[outLoc]} at pos: {outLoc}"
       calc mem (pos + 4)

   | Mul ->
       //printfn $"Pos: {pos} - Performing Mul"
       let inAPos = mem.[pos + 1]
       let inBPos = mem.[pos + 2]
       let outLoc = mem.[pos + 3]
       mem.[outLoc] <- mem.[inAPos] * mem.[inBPos]
       //printfn $"Result: {mem.[outLoc]} at pos: {outLoc}"
       calc mem (pos + 4)

   | Exit ->
       //printfn $"Pos: {pos} - Exit"
       mem

   | Unknown x ->
       failwithf $"Unknown opcode ${x} at position ${pos}"

let runCalc mem v1 v2 =
    let mem = Array.copy mem
    mem.[1] <- v1
    mem.[2] <- v2
    calc mem 0

let findVariables mem target =
    let limit = (Array.length mem) - 1
    let possibilities =
        seq { for noun in 1..limit do
                  for verb in 1..limit do
                      noun,verb }
    let (noun, verb) =
        possibilities
        |> Seq.find (fun (noun, verb) ->
            let calcResult = runCalc mem noun verb
            //printfn $"CalcResult {calcResult.[0]} for inputs {(noun, verb)}"
            target = calcResult.[0])

    (100 * noun) + verb

let part1 = runCalc startingMem 12 2

printfn $"Part 1: {part1.[0]}"

printfn $"Part 2: {findVariables startingMem 19690720}"
