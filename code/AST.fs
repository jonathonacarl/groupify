module AST

type Expr = 
| Num of float
| Element of string
| Operation of string
| Group of Expr*ExprSet
and ExprSet = 
| Integers of string
| Rationals of string
| Reals of string
| Complex of string
| Numbers of Expr list
| Elements of Expr list