#I __SOURCE_DIRECTORY__

open System.IO
open System.Collections.Generic


let flip f a b = f b a
let mkTuple x y = x, y
let tee f x = f x; x


let public readInput inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines

let public readLinesWithHashComments inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines
    |> Seq.filter (fun (str : System.String) -> not (str.StartsWith('#')))

let public readLinesWithSlashComments inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines
    |> Seq.filter (fun (str : System.String) -> not (str.StartsWith("//")))

let (|KeyValue|) (keyValuePair : KeyValuePair<'k, 'v>) : 'k * 'v =
    let k = keyValuePair.Key
    let v = keyValuePair.Value
    (k, v)

module Tuple =
    let flip (x,y) = y, x

module Map =
    let extendListValue key value m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key [value]
        | Some existing -> m |> Map.add key (value::existing)

    let ofOneToManySeq xs : Map<'a, 'b list> =
        Seq.fold (fun m (x,y) -> m |> extendListValue x y) Map.empty xs

    let flattenOneToMany m =
        let flatten = (fun (KeyValue(x, ys)) -> ys |> Seq.map (mkTuple x))
        Seq.collect flatten m

module List =
    let permutationsWithReplacement (values : 'a list) times =
        let splitValues = values |> List.map List.singleton
        let folder acc values =
            List.allPairs acc values
            |> List.map (fun (xs, x) -> x::xs)
        List.fold folder splitValues (List.replicate (times - 1) values)

    let rec permutations (values : Set<'a>) count : 'a list list =
        if count = 0 then [[]]
        else
            values |> List.ofSeq
            |> List.collect (fun x ->
               permutations (Set.remove x values) (count - 1)
               |> List.map (fun xs -> x::xs))

    let rec combinations n l =
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) ->
            List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

module Seq =
    let groupByTuple (xs : ('a * 'b) seq) =
        xs
        |> Seq.groupBy fst
        |> Seq.map (fun (k,v) -> k, v |> Seq.map snd)
        |> Map

module String =
    let split (delimiter : char) (input : string) =
        input.Split delimiter