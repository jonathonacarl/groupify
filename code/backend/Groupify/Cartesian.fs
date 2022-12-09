module Cartesian

(*
 * Helper method that generates tuples of all
 * permutations in the set of elements provided. 
 *)
let rec cartesianProduct xs ys =
    match xs,ys with
    | [] , _ -> []
    | _ , [] -> []
    | x::xes , _ ->
        let zs = ys |> List.map (fun y -> (x,y))
        zs @ cartesianProduct xes ys 

(*
 * Note: the length of this list should be n^3 since for each
 * slot in the 3-tuple, we have n choices.
 *)
let cartesianTriples xs ys zs =
    let pairs = cartesianProduct xs ys
    let triples = []

    let rec helper zs triples =
        match zs, triples with
        | [], _ -> triples
        | z::zes, _ ->
            let tripsSoFar = pairs |> List.map(fun(x,y) -> (x,y,z))
            let newTrips = tripsSoFar@triples
            helper zes newTrips

    helper zs triples
