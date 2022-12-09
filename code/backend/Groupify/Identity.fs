module Identity

open AST
open Cartesian

(*
 * Verify existence of identity element in the
 * provided set under given binary operation.
 *)

let identity e op = 
    match e with 
    | Num(_) -> (false, float -1)
    | Element(_) -> (false, float -1)
    | Operation(_) -> (false, float -1)
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


        let rec idUtil soFar =
            match soFar with
            | [] -> (false, float -1)
            | x::xs ->
                let pairs = cartesianProduct [x] newNums

                let rec identityHelper pairs nums id =
                    match pairs with 
                    | [] -> true
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
                        
                        if (id = n && res1 = m && res2 = m) || (id = m && res1 = n && res2 = n) then
                            identityHelper pairs2 nums id
                        else 
                            false  
                if identityHelper pairs newNums x then
                    (true, x)
                else
                    idUtil xs 
        idUtil newNums
            
    | Group(_, _) -> failwith "Not Implemented" 
