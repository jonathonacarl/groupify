module Associative

open AST
open Cartesian

(*
 * Verify associativity holds for any triplet in the
 * provided set under given binary operation.
 *)
let associativity e op =
    
    match e with 
    | Num(_) -> false
    | Element(_) -> false
    | Operation(_) -> false
    | Group(Operation(_),nums) ->

        let newNums = 
            match nums with 
            | Integers(_) -> []
            | Rationals(_) -> []
            | Reals(_) -> []
            | Complex(_) -> []
            | Numbers(ns) -> ns |> List.map (fun(z) -> 
                    match z with 
                    | Num(n) -> float n
                    | Operation(_) -> 0.0
                    | Element(_) -> 0.0
                    | Group(_,_) -> 0.0
                )
            | Elements(e) -> []

        let triples = cartesianTriples newNums newNums newNums

        let rec assocUtil triples = 
            match triples with
            | [] -> true
            | (a,b,c)::ts ->

                let first, second = 
                    match op with
                    | "+" -> (a+b) + c, a + (b+c)
                    | "-" -> (a-b) - c, a - (b-c)
                    | "*" -> (a*b) * c, a * (b*c)
                    | "/" -> (a/b) / c, a / (b/c)
                    | _ ->
                        let numToModBy = float(string(op[2]))
                        (((a + b) % numToModBy) + c) % numToModBy, (a + ((b + c) % numToModBy)) % numToModBy
                if (first <> second) then
                    false
                else
                    assocUtil ts

        assocUtil triples
    
    | Group(_, _) -> failwith "Not Implemented"