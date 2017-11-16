module Values

open Syntax

type 'v env = (string * 'v) list

type value =
    | ValI of int
    | ValB of bool
    | ValR of float
    | ValC of char
    | ValS of string
    | ValT of value list
    | ValL of value list
    | Closure of string * string list * typexpr * value env

let rec lookup env x =
    match env with
        | [] -> failwith (x + " not found")
        | (e, v)::env -> if x = e then v else lookup env x

let rec mem x xs =
    match xs with
        | [] -> false
        | y::xs -> if x = y then true else (mem x xs)

let rec stringOfValue v =
    match v with
        | ValI i -> string i
        | ValB b -> string b 
        | ValR r -> string r
        | ValC c -> string c
        | ValS s -> s
        | ValT vs -> "(" + (List.map stringOfValue vs |> String.concat ", ") + ")"
        | ValL vs -> "[" + (List.map stringOfValue vs |> String.concat ", ") + "]"
        | Closure(fname, args, body, fdecl) -> failwith "Cannot represent a closure properly"