module Evaluator

open AST
open Parser

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
 * Verify closure of elements in the
 * provided set under given binary operation.
 *)
let closure e =

    match e with 
    | Num(c) -> false
    | Operation(a,b,c) -> false
    | Group(Operation(plus,m,numToModBy),nums) ->

        let newNums = nums |> List.map (fun (Num(z)) -> z)
        let pairs = cartesianProduct newNums newNums 
        (*
         * Helper method that returns true iff the combination
         * of any two elements exists in the provided set, false otherwise.
         *)
        let rec closureHelper pairs nums =
            match pairs with 
            | [] -> true
            | (n,m)::pairs2 -> 
                let res = (n+m) % numToModBy
                if (nums |> List.contains res) then
                    closureHelper pairs2 nums
                else 
                    false 
        closureHelper pairs newNums

    | Group(_, _) -> failwith "Not Implemented"
    

(*
 * Verify existence of identity element in the
 * provided set under given binary operation.
 *)

let identity e = 
    match e with 
    | Num(c) -> (false, -1)
    | Operation(a,b,c) -> (false, -1)
    | Group(Operation(plus,m,numToModBy),nums) ->
        
        let newNums = nums |> List.map (fun (Num(z)) -> z)

        let rec idUtil soFar =
            match soFar with
            | [] -> (false, -1)
            | x::xs ->
                let pairs = cartesianProduct [x] newNums

                let rec identityHelper pairs nums id =
                    match pairs with 
                    | [] -> true
                    | (n,m)::pairs2 -> 
                        let res = (n+m) % numToModBy
                        if (id = n && res = m) || (id = m && res = n) then
                            identityHelper pairs2 nums id
                        else 
                            false  
                if identityHelper pairs newNums x then
                    (true, x)
                else
                    idUtil xs 
        idUtil newNums
            
    | Group(_, _) -> failwith "Not Implemented" 


(*
 * Verify existence of inverse elements for each element
 * in the provided set under given binary operation.
 *)
let inverses e = 

    match e with 
    | Num(c) -> false
    | Operation(a,b,c) -> false
    | Group(Operation(plus,m,numToModBy),nums) ->

        let _,id = identity e
        let newNums = nums |> List.map (fun (Num(z)) -> z)

        let rec inversesUtil soFar =
            match soFar with
            | [] -> true
            | x::xs ->
                let pairs = cartesianProduct [x] newNums

                let rec inversesHelper pairs =
                    match pairs with 
                    | [] -> false
                    | (n,m)::pairs2 ->
                        let res = (n+m) % numToModBy
                        if res = id then
                            true
                        else 
                            inversesHelper pairs2
            
                if inversesHelper pairs then
                    inversesUtil xs
                else
                    false

        inversesUtil newNums

    | Group(_, _) -> failwith "Not Implemented"

(*
 * Verify associativity holds for any triplet in the
 * provided set under given binary operation.
 *)
// let associativity e =
//     true
// (a + b) + c = a + (b + c)

(*
 * Verify all conditions hold for a set of elements
 * and a binary operation to form a group, i.e
 * 1. Closure
 * 2. Existence of Identity
 * 3. Existence of Inverses
 * 4. Associativity under operation holds
 *)
// let evaluator = 

    // closure && identity && inverses && associativity