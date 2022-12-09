module Inverses

open AST
open Cartesian
open Identity

(*
 * Verify existence of inverse elements for each element
 * in the provided set under given binary operation.
 *)
let inverses e op = 

    match e with 
    | Num(_) -> (None,false)
    | Element(_) -> (None,false)
    | Operation(_) -> (None,false)
    | Group(Operation(_),nums) ->

        let _,id = identity e op
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

        let invList = []

        let rec inversesUtil soFar invList =
            match soFar with
            | [] -> (Some(invList), true)
            | x::xs ->
                let pairs = cartesianProduct [x] newNums

                let rec inversesHelper pairs =
                    match pairs with 
                    | [] -> ([(x,x)], false)
                    | (n,m)::pairs2 ->

                        let res = 
                            match op with
                            | "+" -> n+m
                            | "-" -> n-m
                            | "*" -> n*m
                            | "/" -> n/m
                            | _ ->
                                let numToModBy = float(string(op[2]))
                                (n+m) % numToModBy

                        if res = id then
                            if invList |> List.contains (n,m) || invList |> List.contains (m,n) then
                                (invList, true)
                            else
                                ((n,m)::invList,true)
                        else 
                            inversesHelper pairs2
            
                let newInvList,res = inversesHelper pairs

                if res then
                    inversesUtil xs newInvList
                else
                    let p1,_ = newInvList[0]
                    (Some([p1,p1]), false)

        inversesUtil newNums invList

    | Group(_, _) -> failwith "Not Implemented"
