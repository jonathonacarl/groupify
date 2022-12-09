module Closure

open AST
open Cartesian


(*
 * Verify closure of elements in the
 * provided set under given binary operation.
 *)
let closure e op =

    match e with 
    | Num(_) -> (None,None,false)
    | Element(_) -> (None,None,false)
    | Operation(_) -> (None,None,false)
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

        let pairs = cartesianProduct newNums newNums 
        (*
         * Helper method that returns true iff the combination
         * of any two elements exists in the provided set, false otherwise.
         *)
        let rec closureHelper pairs nums =
            match pairs with 
            | [] -> (None, None, true)
            | (n,m)::pairs2 ->

                let res1, res2 = 
                    match op with
                    | "+" -> n+m, m+n
                    | "-" -> n-m, m-n
                    | "*" -> n*m, m*n
                    | "/" -> n/m, m/n
                    | _ ->
                        let numToModBy = float(string(op[2]))
                        (n+m) % numToModBy, (m+n) % numToModBy

                if (nums |> List.contains res1) then
                    if (nums |> List.contains res2) then
                        closureHelper pairs2 nums
                    else
                        (Some(m), Some(n), false)
                else 
                    (Some(n),Some(m),false)
        closureHelper pairs newNums

    | Group(_, _) -> failwith "Not Implemented"

