module Types

type typ =
    | TypeG
    | TypeI
    | TypeB
    | TypeR
    | TypeC
    | TypeS
    | TypeL of typ
    | TypeT of typ list
    | TypeA of typ list * typ

let rec stringOfType t =
    match t with
        | TypeG -> "'a"
        | TypeI -> "int"
        | TypeB -> "bool"
        | TypeR -> "real"
        | TypeC -> "char"
        | TypeS -> "string"
        | TypeL typ -> "[" + stringOfType typ + "]"
        | TypeT typs -> "(" + (List.map stringOfType typs |> String.concat " * ") + ")"
        | TypeA(tas, tb) -> 
            (List.map stringOfType tas |> String.concat " * ") + " -> " + (stringOfType tb)