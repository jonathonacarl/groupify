module Parser

open Combinator
open AST

(* 
 * Declare expression parser so we can use it recursively 
 *)
let pexpr,pexprImpl = recparser()

(*
 * Parse a single number.
 *)
let pnum = pmany1 pdigit |>> stringify |>> int |>> Num <!> "number"

(*
 * Helper parser to read first two signs of operation.
 *)
let foper = pseq (pchar '+') (pchar '%') (fun (fop,sop) -> string fop,string sop) <!> "first operation"

(*
 * Parse the operation.
 *)
let poper = pseq foper (pdigit) (fun (fop, numOp) -> 
                                    let a,b = fop
                                    Operation(a,b,int(string(numOp))))
                                     <!> "operation"

(*
 * Parse the set of numbers.
 *)
let pset = pbetween (pchar '{') (pchar '}') (pmany1 (pleft (pnum) (pchar ','))) <!> "set"

(*
 * Parse the set and operation.
 *)
let pgroup = pseq (pset) (pright pws1 poper) (fun (nums, oper) -> oper,nums) |>> Group <!> "group"

(*
 * Declare the expression parser implementation.
 *)
pexprImpl := pnum <|> poper <|> pgroup <!> "expr"

(*
 * The complete grammar parser.
 *)
let grammar = pleft pexpr peof <!> "grammer"

(*
 * Verify valid input string is transformed into the Abstract Syntax Tree.
 *)
let parse(s: string) : Expr option = 

    let ast = prepare s 
    match (grammar ast) with 
    | Success(res, _) -> Some res
    | Failure(_,_) -> None

(*
 * Print the Abstract Syntax Tree.
 *)
let rec prettyprint(e) : string = 
   
    e.ToString()