module Evaluator

open AST
open Diagnostic

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
    | Num(_) -> ("invalid expression.", false)
    | Element(_) -> ("invalid expression.", false)
    | Operation(_) -> ("invalid expression.", false)
    | Group(op, set) ->
        let oper = 
            match op with
            | Num(_) -> ""
            | Operation(o) -> o
            | Element(_) -> ""
            | Group(_) -> ""

        diagnostic e set oper