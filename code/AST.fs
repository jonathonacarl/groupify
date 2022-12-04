module AST

type Expr = 
| Num of int
| Operation of string*string*int
| Group of Expr*Expr list