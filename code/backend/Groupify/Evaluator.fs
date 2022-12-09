module Evaluator

open AST
open Parser
open Combinator

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


let validation e op = 
    
    let e1, e2, closed = closure e op
    let identity, id = identity e op
    let invElems, inverse = inverses e op
    let associative = associativity e op

    (e1, e2, closed), (identity, id), (invElems, inverse), (associative)

let doEvaluation e op = 
    
    match op with
        | "+" -> validation e "+"
        | "-" -> validation e "-"
        | "*" -> validation e "*"
        | "/" -> validation e "/"
        | _ -> validation e op


(*
 * Verify all conditions hold for a set of elements
 * and a binary operation to form a group, i.e
 * 1. Closure
 * 2. Existence of Identity
 * 3. Existence of Inverses
 * 4. Associativity under operation holds
 *)
let evaluator e = 
    
    (*
     * Print diagnostic messages to std-out, informing user why a given set and operation
     * is (not) a group, along with useful information about tests performed.
     *)
    let rec evalUtil e set op = 
        match set with
        | Integers(z) ->
            let ret = $"%A{z} is a group under {op} because\n 
                It is closed under {op}\n
                The identity element is 1.\n
                Every element has an inverse.\n
                {op} is associative."
            (ret, true)
        | Rationals(q) ->
            let ret = $"%A{q} is a group under {op} because\n 
                It is closed under {op}\n
                The identity element is 1.\n
                Every element has an inverse.\n
                {op} is associative."
            (ret, true)
        | Reals(r) ->
            let ret = $"%A{r} is a group under {op} because\n 
                It is closed under {op}\n
                The identity element is 1.\n
                Every element has an inverse. \n
                {op} is associative."
            (ret, true)
        | Complex(c) ->
            let ret = $"%A{c} is a group under {op} because\n 
                It is closed under {op}\n
                The identity element is 1 + 0i.\n
                Every element has an inverse.\n
                {op} is associative."
            (ret, true)
        | Numbers(n) ->
            
            let (e1, e2, closed), (isIdentity, id), (invElems, inverse), associative = doEvaluation e op
            
            let res = [(closed, "closed"); (isIdentity, "identity"); (inverse, "inverse"); (associative, "associative")]

            let rec numberSetUtil e res str =

                match e with 
                | Num(_) -> ("",false)
                | Element(_) -> ("",false)
                | Operation(_) -> ("",false)
                | Group(Operation(op),nums) ->
                    match res, str with
                    | [], "" ->
                        match invElems with
                        | Some(invElems) ->
                            let ret = $"%A{nums} is a group under {op} because:\n\nIt is closed under {op}\n\nThe identity element is {id}\n\nEvery element has an inverse: {invElems} \n\n{op} is associative."
                            (ret, true)
                        | _ -> failwith "inverses not implemented correctly"
                    | [], _ -> (str,false)
                    | (x,s)::xs, _ ->
                        if not x then
                            let toAdd = if str.Length = 0 then $"%A{nums} is not a group under {op} because:\n" else ""
                            match s with
                            | "closed" ->
                                match e1,e2 with
                                | Some(e1), Some(e2) ->
                                    let n1 = e1
                                    let n2 = e2
                                    let sFormat = 
                                        match op with
                                        | "+" -> $"{n1} + {n2} = {n1 + n2}"
                                        | "-" -> $"{n1} - {n2} = {n1 - n2}"
                                        | "*" -> $"{n1} * {n2} = {n1 * n2}"
                                        | "/" -> $"{n1} / {n2} = {n1 / n2}"
                                        | _ ->
                                            let numToModBy = float(string(op[2]))
                                            $"({n1} + {n2}) %% {numToModBy} = {(n1 + n2) % numToModBy}"
                                    let newStr = str + toAdd + $"It is not closed. Notice that {n1},{n2} are in %A{nums}, but {sFormat} is not in %A{nums}.\n"
                                    numberSetUtil e xs newStr
                                | _, _ -> failwith "invalid closure implementation."
                            | "identity" ->
                                let newStr = str + toAdd + "It contains no identity element.\n"
                                numberSetUtil e xs newStr
                            | "inverse" ->
                                match invElems with
                                | Some(invElems) ->
                                    let elem = fst(invElems[0])
                                    let newStr = str + toAdd + $"{elem} is an element with no inverse.\n"
                                    numberSetUtil e xs newStr
                                | _ -> failwith "invalid inverse implementation."
                            | "associative" ->
                                let newStr = str + toAdd + "It is not associative."
                                numberSetUtil e xs newStr
                            | _ -> failwith "invalid result array"
                        else
                            numberSetUtil e xs str
                | Group(_, _) -> failwith "Not Implemented"

            numberSetUtil e res ""

        | Elements(e2) ->
            let ret = $"%A{e2} is a group under {op} because\n 
                It is closed under {op}\n
                The identity element is {id}\n
                Every element has an inverse. \n
                {op} is associative."
            (ret, true)
    
    match e with
    | Num(_) -> ("", false)
    | Element(_) -> ("", false)
    | Operation(_) -> ("", false)
    | Group(op, set) ->
        let oper = 
            match op with
            | Num(_) -> ""
            | Operation(o) -> o
            | Element(_) -> ""
            | Group(_) -> ""

        evalUtil e set oper