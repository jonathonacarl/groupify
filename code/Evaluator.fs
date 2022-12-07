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

(*
 * Verify closure of elements in the
 * provided set under given binary operation.
 *)
let closure e =

    match e with 
    | Num(c) -> (None,None,false)
    | Operation(a,b,c) -> (None,None,false)
    | Group(Operation(plus,m,numToModBy),nums) ->

        let newNums = nums |> List.map (fun (Num(z)) -> z)
        let pairs = cartesianProduct newNums newNums 
        (*
         * Helper method that returns true iff the combination
         * of any two elements exists in the provided set, false otherwise.
         *)
        let rec closureHelper pairs nums =
            match pairs with 
            | [] -> (None, None, true)
            | (n,m)::pairs2 -> 
                let res1 = (n+m) % numToModBy
                let res2 = (m+n) % numToModBy
                if (nums |> List.contains res1 && nums |> List.contains res2) then
                    closureHelper pairs2 nums
                else 
                    (Some(n),Some(m),false)
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
    | Num(c) -> (None,false)
    | Operation(a,b,c) -> (None,false)
    | Group(Operation(plus,m,numToModBy),nums) ->

        let _,id = identity e
        let newNums = nums |> List.map (fun (Num(z)) -> z)
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
                        let res = (n+m) % numToModBy
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

(*
 * Verify associativity holds for any triplet in the
 * provided set under given binary operation.
 *)
let associativity e =
    
    match e with 
    | Num(c) -> false
    | Operation(a,b,c) -> false
    | Group(Operation(plus,m,numToModBy),nums) ->

        let newNums = nums |> List.map (fun (Num(z)) -> z)
        let triples = cartesianTriples newNums newNums newNums

        let rec assocUtil triples = 
            match triples with
            | [] -> true
            | (a,b,c)::ts ->
                let first = (((a + b) % numToModBy) + c) % numToModBy
                let second = (a + ((b + c) % numToModBy)) % numToModBy
                if (first <> second) then
                    false
                else
                    assocUtil ts

        assocUtil triples
    
    | Group(_, _) -> failwith "Not Implemented"

(*
 * Verify all conditions hold for a set of elements
 * and a binary operation to form a group, i.e
 * 1. Closure
 * 2. Existence of Identity
 * 3. Existence of Inverses
 * 4. Associativity under operation holds
 *)
let evaluator e = 

    match e with 
    | Num(c) -> ("",false)
    | Operation(a,b,c) -> ("",false)
    | Group(Operation(plus,m,numToModBy),nums) ->

        let e1,e2,closed = closure e
        let identity, id = identity e
        let invElems,inverse = inverses e
        let associative = associativity e

        let res = [(closed, "closed"); (identity, "identity"); (inverse, "inverse"); (associative, "associative")]
        let str = ""
        
        let rec evalUtil res str = 
            match res, str with
            | [], "" ->
                match invElems with
                | Some(invElems) ->
                    let ret = $"%A{nums} is a group under {plus + m + string numToModBy} because\n 
                    It is closed under {plus + m + string numToModBy}\n
                    The identity element is {id}\n
                    Every element has an inverse: {invElems} \n
                    {plus + m + string numToModBy} is associative."
                    (ret, true)
                | _ -> failwith "inverses not implemented correctly"
            | [], _ -> (str,false)
            | (x,s)::xs, _ ->
                if not x then
                    let toAdd = if str.Length = 0 then $"%A{nums} is not a group under {plus + m + string numToModBy} because:\n" else ""
                    match s with
                    | "closed" ->
                        match e1,e2 with
                        | Some(e1), Some(e2) ->
                            let n1 = e1
                            let n2 = e2
                            let newStr = str + toAdd + $"It is not closed. Notice that {n1},{n2} are in %A{nums}, but {n1} + {n2} %% {numToModBy} = {n1 + n2 % numToModBy} is not in %A{nums}.\n"
                            evalUtil xs newStr
                        | _, _ -> failwith "invalid closure implementation."
                    | "identity" ->
                        let newStr = str + toAdd + "It contains no identity element.\n"
                        evalUtil xs newStr
                    | "inverse" ->
                        match invElems with
                        | Some(invElems) ->
                            let elem = invElems[0]
                            let newStr = str + toAdd + $"{elem} is an element with no inverse.\n"
                            evalUtil xs newStr
                        | _ -> failwith "invalid inverse implementation."
                    | "associative" ->
                        let newStr = str + toAdd + "is not associative.\n"
                        evalUtil xs newStr
                    | _ -> failwith "invalid result array"
                else
                    evalUtil xs str

        evalUtil res str
    | Group(_, _) -> failwith "Not Implemented"