module Typechecker

open Types
open Syntax
open Values

let ifBranchTypeMsg = "All branches of an 'if' expression must return the same type."

let rec typecheck (expr : typexpr) (env : typ env) =

    let unifyTypeG t1 t2 exptType =
        match (t1, t2) with
            | (TypeL TypeG, TypeL t) -> TypeL t
            | (TypeL t, TypeL TypeG) -> TypeL t
            | (t1, t2) when t1 = t2  -> t1
            | _ ->

                let expecT = t1 |> stringOfType
                let actualT = t2 |> stringOfType

                failwithf "%sThis expression was expected to have type %s but here has type %s\n" exptType expecT actualT
   
    match expr with
        | ConstN   -> TypeL TypeG
        | ConstI _ -> TypeI
        | ConstB _ -> TypeB
        | ConstR _ -> TypeR
        | ConstC _ -> TypeC
        | ConstS _ -> TypeS
        | Var x -> lookup env x
        | Sel(i, xs) ->

            let txs = typecheck xs env

            match txs with
                | TypeT xs -> 
                    let sxs = List.length xs

                    if mem i [1..sxs] then (List.item (i - 1) xs)
                    else 
                        failwithf "Error applying selector # of value %d, to a tuple of length %d\n" i sxs

                | _ -> failwith "Selector # must be applied to a tuple\n"


        | Tup xs -> TypeT (List.map (fun x -> typecheck x env) xs)
        | List [] -> TypeL TypeG
        | List xs -> 

            let txsSet = List.map (fun x -> typecheck x env) xs |> Set.ofList
            let txsList = txsSet |> Set.toList
            let expecT = List.item 0 txsList

            if Set.count txsSet = 1 then TypeL expecT
            else

                let actualT = List.item 1 txsList
                failwithf "This expression was expected to have type %s but here has type %s\n" (expecT |> stringOfType) (actualT |> stringOfType)

        | Cons(e, ConstN) -> TypeL (typecheck e env)
        | Cons(e1, e2) -> 

            let t1 = typecheck e1 env
            let t2 = typecheck e2 env

            match (t1, t2) with
                | (t, TypeL tt) when t = tt -> TypeL t1
                | _ -> 
                    failwithf "Type mismatch. Expecting a %s but given a %s\nThis expression was expected to have type %s but here has type %s\n" (TypeL t1 |> stringOfType) (TypeL t2 |> stringOfType) (t1 |> stringOfType) (t2 |> stringOfType)

        | Let(xdefs, body) -> 

            let xs, xsvals = List.unzip xdefs
            let txs = List.map (fun x -> typecheck x env) xsvals
            let extendedEnv = (List.zip xs) txs @ env

            typecheck body extendedEnv

        | Prim(op, e1, e2) ->

            let te1 = typecheck e1 env
            let te2 = typecheck e2 env

            match (op, te1, te2) with

                | ("+", TypeI, TypeI) -> TypeI
                | ("-", TypeI, TypeI) -> TypeI
                | ("*", TypeI, TypeI) -> TypeI
                | ("/", TypeI, TypeI) -> TypeI
                | ("%", TypeI, TypeI) -> TypeI

                | ("+", TypeR, TypeR) -> TypeR
                | ("-", TypeR, TypeR) -> TypeR
                | ("*", TypeR, TypeR) -> TypeR
                | ("/", TypeR, TypeR) -> TypeR

                | ("+", TypeS, TypeS) -> TypeS
                | ("^", TypeS, TypeS) -> TypeS

                | ("*", TypeS, TypeI) -> TypeS
                | ("*", TypeI, TypeS) -> TypeS

                | ("&&", TypeB, TypeB) -> TypeB
                | ("||", TypeB, TypeB) -> TypeB

                | ("<", TypeI, TypeI)  -> TypeB
                | ("<=", TypeI, TypeI) -> TypeB
                | (">", TypeI, TypeI)  -> TypeB
                | (">=", TypeI, TypeI) -> TypeB
                | ("=", TypeI, TypeI)  -> TypeB
                | ("<>", TypeI, TypeI) -> TypeB

                | ("<", TypeR, TypeR)  -> TypeB
                | ("<=", TypeR, TypeR) -> TypeB
                | (">", TypeR, TypeR)  -> TypeB
                | (">=", TypeR, TypeR) -> TypeB
                | ("=", TypeR, TypeR)  -> TypeB
                | ("<>", TypeR, TypeR) -> TypeB

                | ("<", TypeC, TypeC)  -> TypeB
                | ("<=", TypeC, TypeC) -> TypeB
                | (">", TypeC, TypeC)  -> TypeB
                | (">=", TypeC, TypeC) -> TypeB
                | ("=", TypeC, TypeC)  -> TypeB
                | ("<>", TypeC, TypeC) -> TypeB

                | ("<", TypeS, TypeS)  -> TypeB
                | ("<=", TypeS, TypeS) -> TypeB
                | (">", TypeS, TypeS)  -> TypeB
                | (">=", TypeS, TypeS) -> TypeB
                | ("=", TypeS, TypeS)  -> TypeB
                | ("<>", TypeS, TypeS) -> TypeB
                
                | _ ->
                
                    let strTe1 = stringOfType te1
                    let strTe2 = stringOfType te2

                    failwithf "Error applying operator %s to expressions of types: %s and %s\n" op strTe1 strTe2

        | If(e1, e2, e3) ->

            let te1 = typecheck e1 env

            match te1 with 
                | TypeB ->

                    let te2 = typecheck e2 env 
                    let te3 = typecheck e3 env

                    unifyTypeG te2 te3 ifBranchTypeMsg

                | typany -> 

                    let expecT = TypeB |> stringOfType
                    let actualT = typany |> stringOfType

                    failwithf "This expression was expected to have type %s but here has type %s\n" expecT actualT

        | Letfun(fname, args, tretrn, fbody, letbody) ->

            let xs, txs = List.unzip args
            let tf = TypeA(txs, tretrn)
            let fBodyEnv = args @ ((fname, tf) :: env)
            let letBodyEnv = (fname, tf) :: env
            let fCheckedType = typecheck fbody fBodyEnv

            if fCheckedType = tretrn then typecheck letbody letBodyEnv
            else 
                failwithf "This expression was expected to have type %s but here has type %s\n" (stringOfType tretrn) (stringOfType fCheckedType)

        | Call(Var fname, args) ->

            let tf = lookup env fname

            match tf with
                | TypeA(txs, tb) ->

                    let targs = List.map (fun x -> typecheck x env) args
                    let zippedTypes = List.zip txs targs

                    let customErrorMsg = sprintf "Error applying function %s" fname
                    let unifiedTypes = List.map (fun x -> unifyTypeG (fst x) (snd x) "") zippedTypes

                    if unifiedTypes = txs then tb
                    else

                        let expecT = List.map stringOfType txs |> String.concat " * "
                        let actualT = List.map stringOfType targs |> String.concat " * "

                        failwithf "Error applying function %s. It has expects: %s, while it was given a type: %s\n" fname expecT actualT

                | _ -> failwithf "The value or constructor %s is not defined" fname

        | Call _ -> failwith "Not a first-order function\n"

        | Match(e0, e1, (x, xs, e2)) ->

            let te0 = typecheck e0 env

            match te0 with
                | TypeL t ->

                    let extendedEnv = (x, t) :: (xs, TypeL t) :: env

                    let te1 = typecheck e1 extendedEnv
                    let te2 = typecheck e2 extendedEnv

                    unifyTypeG te1 te2 ""

                | _ ->
                    failwithf "This expression was expected to have type %s but here has type %s\n" (stringOfType (TypeL TypeG)) (stringOfType te0)

        | Range(low, Some step, high) ->

            let tl = typecheck low env
            let ts = typecheck step env
            let th = typecheck high env

            match (tl, ts, th) with
                | (TypeI, TypeI, TypeI) -> TypeL TypeI
                | _ -> failwith "Error while typing range expression\n"

        | Range(low, None, high) ->

            let tl = typecheck low env
            let th = typecheck high env

            match (tl, th) with
                | (TypeI, TypeI) -> TypeL TypeI
                | _ -> failwith "Error while typing range expression\n"