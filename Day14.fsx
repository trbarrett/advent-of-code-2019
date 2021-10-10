#load "./Helper.fsx"

// Day 14 - Calculate how much ORE is needed for a chemical reaction
//
// Method - By inverting the tree and performing a post-order tree traversal we
//          can quickly calculate the amount of ore we need at each level. The
//          only hiccup is we need to carry around extra materials we don't use
//          and make sure to add and remove from it correctly.
//
//          For part 2 I used an infinite exponential sequence to find an upper
//          and lower bound to how much fuel I could create, then I used binary
//          search to hone in on the final amount.

open Helper

type Chem = { Name : string; Amount : int64 }
type Reaction = { In : seq<Chem>; Out : Chem }
type ReverseReactionMap = Map<string, Chem * seq<Chem>>

let splitReactionStr str =
    let [| input; output |] = String.splitStr "=>" str
    input, output

let splitChems = String.splitStr ","

let parseChemicalAndAmount str =
    let [|amount; name|] = str |> String.trim |> String.splitStr " "
    { Name = name; Amount = int64 amount }

let parseReactionChems (input, output) =
   { In = input |> splitChems |> Seq.map parseChemicalAndAmount
     Out = parseChemicalAndAmount output }


let addReaction map reaction =
    let { In = inputs; Out = output } = reaction
    Map.add output.Name (output, inputs) map

let buildReverseReactionMap reactions : ReverseReactionMap =
    let addReaction map { In = inputs; Out = output } =
        Map.add output.Name (output, inputs) map
    Seq.fold addReaction Map.empty reactions

let rec getCostOfFuel' (reactionMap : ReverseReactionMap) storeHouse need =
    match need.Name with
    | "ORE" -> need.Amount, storeHouse
    | _ ->
        let out, inputs = Map.find need.Name reactionMap

        // if we have some in inventory already, lets use those first
        let inventory = Map.tryFind need.Name storeHouse |> Option.defaultValue 0L
        let needAmount = need.Amount - inventory
        if needAmount <= 0L
        then
            // we didn't use any ore for this reaction, since we didn't run it
            0L,
            // we had more in inventory than needed. Put it back
            Map.add out.Name (abs needAmount) storeHouse
        else
            let storeHouse = Map.add need.Name 0L storeHouse // remove the amount we used from inventory

            // How many times do we need to run this reaction to get
            // the amount we need? (e.g. if we need 15A and we produce 7A, we need
            // to run this reaction 3 times to get what we need
            let runCount = int64 (ceil(float needAmount / float out.Amount))
            let usedOre, storeHouse =
                ((0L, storeHouse),inputs)
                ||> Seq.fold (fun (accOreCost, storeHouse) part ->
                    // now how many input chems do we need to get the reaction we need
                    // e.g. we need 32 B, and the reaction is 5 A => 10 B, we need 4 lots
                    // of 5, or 20 A's
                    let howMany = runCount * part.Amount

                    let oreCost, storeHouse =
                        getCostOfFuel' reactionMap
                                       storeHouse
                                       { Name = part.Name; Amount = howMany }
                    //printfn $"Cost of {chem.Name} = {cost / chem.Amount}"
                    accOreCost + oreCost, storeHouse)

            // if we have more than needed, let's put it in the store house
            let extras = (runCount * out.Amount) - needAmount
            usedOre, Map.add need.Name extras storeHouse

let getCostOfFuel reactions amount =
    let reactionMap = buildReverseReactionMap reactions
    let usedOre, _ =
        getCostOfFuel' reactionMap Map.empty { Name = "FUEL"; Amount = amount }
    usedOre

let rec binarySearch' fn target lower upper =
    if upper = lower then upper
    else
        let middle = lower + ((upper - lower) / 2L)
        let middleValue = fn middle
        match sign <| compare target middleValue with
          | 0  -> middle
          | -1 -> binarySearch' fn target lower (middle)
          | _  -> binarySearch' fn target (middle+1L) upper

let rec binarySearch fn target lower upper =
    // the binary search will find a value close to the target value if
    // it can't get the actual one, but it might be above the target which
    // is to high for us, so subtract 1 if it is
    let result = binarySearch' fn target lower upper
    let close = fn result
    if close > target then result - 1L else result

let findFuelAmountForOre reactions =
    let availableOre = 1_000_000_000_000L
    // First - find an upper limit to the amount of fuel we'll need
    // create an exponential sequence to find the upper limit quickly
    let expSeq = Seq.initInfinite (pown 2L)
    let upperLimitIndex =
        expSeq
        |> Seq.findIndex (fun x -> (getCostOfFuel reactions x) > availableOre)
    let upperLimit = pown 2L upperLimitIndex
    let lowerLimit = pown 2L (upperLimitIndex-1)
    // Now do a binary search between our limits for the right value
    binarySearch (getCostOfFuel reactions) availableOre lowerLimit upperLimit

let inputReactions =
    Helper.readLinesWithSlashComments "day14.txt"
    |> Seq.map (splitReactionStr >> parseReactionChems)
    |> Seq.toList

let part1 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let usedOre = getCostOfFuel inputReactions 1L
    printfn $"Part1 result: {usedOre}, took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: Part1 result: 579797, took: 7m

let part2 () =
    let sw = System.Diagnostics.Stopwatch.StartNew ()
    let fuelAmount = findFuelAmountForOre inputReactions
    printfn $"Part2 result: {fuelAmount}, took: {sw.ElapsedMilliseconds}ms"
    // Correct answer: Part2 result: 2521844, took: 82ms

part1 ()
part2 ()
