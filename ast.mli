type ast = 
    | PROGRAM of ast list
    | ASSIGNMENT of string * ast
    | READCALL of string list
    | WRITECALL of ast list
    | NUMBER of int
    | VAR of string
    | ADD of ast * ast
    | SUB of ast * ast
