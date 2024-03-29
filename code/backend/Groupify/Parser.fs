module Parser

open Combinator
open AST

(* 
 * Declare expression parser so we can use it recursively 
 *)
let pexpr,pexprImpl = recparser()

(*
 * Parse a negative number.
 *)
let pnegative = pseq (pchar '-') (pmany1 pdigit) (fun(negative, nums) -> negative::nums) <!> "pnegative"

(*
 * Parse a positive number.
 *)
let ppositive = pmany1 pdigit <!> "ppositive"

(*
 * Parse the integers.
 *)
let pintegers = pchar 'Z' |>> string |>> Integers <!> "pintegers"
 
(*
 * Parse the rational numbers.
 *)
let prationalsnonzero = pseq (pchar 'Q') (pchar '*') (fun(a,b) -> string a + string b) |>> Rationals <!> "prationalsnonzero"

let prationalszero = pchar 'Q' |>> string |>> Rationals <!> "prationalszero"

let prationals = prationalsnonzero <|> prationalszero <!> "prationals"

(*
 * Parse the real numbers.
 *)
let prealnonzero = pseq (pchar 'R') (pchar '*') (fun(a,b) -> string a + string b) |>> Reals <!> "prealnonzero"

let prealzero = pchar 'R' |>> string |>> Reals <!> "prealzero"

let preal = prealnonzero <|> prealzero <!> "preal"

(*
 * Parse the complex numbers.
 *)

let pcomplexnonzero = pseq (pchar 'C') (pchar '*') (fun(a,b) -> string a + string b) |>> Complex <!> "pcomplexnonzero"

let pcomplexzero = pchar 'C' |>> string |>> Complex <!> "pcomplexzero"

let pcomplex = pcomplexzero <|> pcomplexzero <!> "pcomplex"

(*
 * Parse a single number, either negative or positive.
 *)
let pnum = pnegative <|> ppositive |>> stringify |>> float |>> Num <!> "pnum"

(*
 * Parse the addition modulo operation.
 *)
let fmodoper = pseq (pchar '+') (pchar '%') (fun (fop,sop) -> string fop,string sop) <!> "fmodoper"

let modoper = pseq fmodoper (pdigit) (fun (fop, numOp) -> 
                                    let a,b = fop
                                    Operation(a + b + string(numOp)))
                                     <!> "modoper"
(*
 * Parse the addition operation.
 *)
let aoper = pchar '+' |>> string |>> Operation <!> "aoper"

(*
 * Parse the subtraction operation.
 *)
let soper = pchar '-' |>> string |>> Operation <!> "soper"

(*
 * Parse the multiplication operation.
 *)
let moper = pchar '*' |>> string |>> Operation <!> "moper"

(*
 * Parse the division operation.
 *)
let doper = pchar '/' |>> string |>> Operation <!> "doper"

(*
 * Parse the operation.
 *)
let poper = modoper <|> aoper <|> soper <|> moper <|> doper <!> "poper"

(*
 * Parse a set of numbers.
 *)
let pnums = (pseq (pmany1 (pleft (pnum) (pchar ','))) (pnum) (fun (nums, num) -> nums @ [num] )) |>> Numbers <!> "pnums"

(*
 * Parse the set.
 *)
let pset = pbetween (pchar '{') (pchar '}') (pnums) <|> (pintegers <|> prationals <|> preal <|> pcomplex) <!> "pset"

(*
 * Parse the set and operation.
 *)
let pgroup = pseq (pset) (pright pws1 poper) (fun (nums, oper) -> oper,nums) |>> Group <!> "pgroup"

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