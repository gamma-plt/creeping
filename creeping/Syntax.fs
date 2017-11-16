module Syntax

open Types

type typexpr =
    | ConstN
    | ConstI of int
    | ConstB of bool
    | ConstR of float
    | ConstC of char
    | ConstS of string
    | Var of string
    | Sel of int * typexpr
    | Tup of (typexpr list)
    | List of (typexpr list) 
    | Cons of typexpr * typexpr
    | Let of (string * typexpr) list * typexpr
    | Prim of string * typexpr * typexpr
    | If of typexpr * typexpr * typexpr
    | Letfun of string * (string * typ) list * typ * typexpr * typexpr
    | Call of typexpr * (typexpr list)
    | Match of typexpr * typexpr * (string * string * typexpr)
    | Range of typexpr * typexpr option * typexpr