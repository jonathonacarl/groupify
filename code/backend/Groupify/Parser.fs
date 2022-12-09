module Parser

open Combinator
open AST

(* 
 * Declare expression parser so we can use it recursively 
 *)
let pexpr,pexprImpl = recparser()

let pnegative = pseq (pchar '-') (pmany1 pdigit) (fun(negative, nums) -> negative::nums) <!> "pnegative"

let ppositive = pmany1 pdigit <!> "ppositive"

let pintegers = pchar 'Z' |>> string |>> Integers <!> "pintegers"
 
let prationals = pchar 'Q' |>> string |>> Rationals <!>"prationals"

let preal = pchar 'R' |>> string |>> Reals <!> "preal"

let pcomplex = pchar 'C' |>> string |>> Complex <!> "pcomplex"

(*
 * Parse a single number, either negative or positive.
 *)
let pnum = pnegative <|> ppositive |>> stringify |>> float |>> Num <!> "pnum"

(*
 * Helper parser to read first two signs of addition modulo operation.
 *)
let fmodoper = pseq (pchar '+') (pchar '%') (fun (fop,sop) -> string fop,string sop) <!> "fmodoper"

(*
 * Addition modulo operation parser.
 *)
let modoper = pseq fmodoper (pdigit) (fun (fop, numOp) -> 
                                    let a,b = fop
                                    Operation(a + b + string(numOp)))
                                     <!> "modoper"

let aoper = pchar '+' |>> string |>> Operation <!> "aoper"

let soper = pchar '-' |>> string |>> Operation <!> "soper"

let moper = pchar '*' |>> string |>> Operation <!> "moper"

let doper = pchar '/' |>> string |>> Operation <!> "doper"

let poper = modoper <|> aoper <|> soper <|> moper <|> doper <!> "poper"

let pnums = (pseq (pmany1 (pleft (pnum) (pchar ','))) (pnum) (fun (nums, num) -> nums @ [num] )) |>> Numbers <!> "pnums"

(*
 * Parse the set.
 *)
let pset = pbetween (pchar '{') (pchar '}') (pnums) <|> (pintegers <|> prationals <|> preal <|> pcomplex) <!> "set"

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
let grammar = pleft pexpr peof <!> "grammar"

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