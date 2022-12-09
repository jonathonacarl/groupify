module Diagnostic

open AST
open Closure
open Identity
open Inverses
open Associative

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
 * Print diagnostic messages to std-out, informing user why a given set and operation
 * is (not) a group, along with useful information about tests performed.
 *)
let rec diagnostic e set op = 
    match set with
    | Integers(z) ->
        let retStr, ret = 
            match op with
            | "+" -> 
                $"""
                %A{z.ToString()} is a group under {op} because:

                It is closed under {op}.

                The identity element is 0.

                Every element has an inverse.
                
                {op} is associative.

                """, true
            | _ -> 
                $"""
                %A{z.ToString()} is not a group under {op} because not every element has an inverse.

                """, false
        (retStr, ret)
    | Rationals(q) ->
        let retStr, ret  = 

            match q with
                | "Q*" -> 

                    match op with
                    | "*" -> 
                        $"""
                        %A{q.ToString()} is a group under {op} because:

                        It is closed under {op}.

                        The identity element is 1.

                        Every element has an inverse.

                        {op} is associative.

                        """, true
                    | _ -> 
                        $"""
                        %A{q.ToString()} is not a group under {op} because it is not closed.   

                        """, false
                | "Q" -> 

                    match op with
                    | "+" ->
                        $"""
                        %A{q.ToString()} is a group under {op} because:

                        It is closed under {op}.

                        The identity element is 0.

                        Every element has an inverse.

                        {op} is associative.

                        """, true
                    | _ ->
                        $"""
                        %A{q.ToString()} is not a group under {op} 0 does not have an inverse.

                        """, false
                | _ ->
                    $"""
                    parser failed.

                    """, false
        (retStr, ret)
    | Reals(r) ->
        let retStr, ret =
            match r with
            | "R*" -> 
                match op with
                | "*" -> 
                    $"""
                    %A{r.ToString()} is a group under {op} because:

                    It is closed under {op}.

                    The identity element is 1.

                    Every element has an inverse.

                    {op} is associative.

                    """, true
                | _ -> 
                    $"""
                    %A{r.ToString()} is not a group under {op} because it is not closed.

                    """, false
            | "R" ->
                match op with
                | "+" ->
                    $"""
                    %A{r.ToString()} is a group under {op} because:

                    It is closed under {op}.

                    The identity element is 0.

                    Every element has an inverse.

                    {op} is associative.

                    """, true
                | _ -> 
                    $"""
                    %A{r.ToString()} is not a group under {op} because 0 does not have an inverse.

                    """, false

            | _ -> 
                $"""
                parser failed.

                """, false

        (retStr, false)
    | Complex(c) ->
        let retStr, ret = 
            match c with
            | "C*" ->
                match op with
                | _ -> 
                    $"""
                    %A{c.ToString()} is a group under {op} because:

                    It is closed under {op}.

                    The identity element is 1.

                    Every element has an inverse.

                    {op} is associative.

                    """, true
            | "C" -> 
                match op with
                | "+" -> 
                    $"""
                    %A{c.ToString()} is a group under {op} because:

                    It is closed under {op}.

                    The identity element is 0.

                    Every element has an inverse.

                    {op} is associative.

                    """, true
                | _ -> 
                    $"""
                    %A{c.ToString()} is not a group under {op} because it is not closed.

                    """, false
            | _ -> 
                $"""
                parser failed.

                """, false

        (retStr, ret)
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
                        let ret = 
                            $"""
                            %A{nums} is a group under {op} because:
                            
                            It is closed under {op}.
                            
                            The identity element is {id}.
                            
                            Every element has an inverse: {invElems}.
                            
                            {op} is associative.

                            """
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
        let retStr, ret = 
            $"""
            %A{e2} is a group under {op} because:

            It is closed under {op}.

            The identity element is {id}.
            
            Every element has an inverse.

            {op} is associative.
            """, true
        (retStr, ret)