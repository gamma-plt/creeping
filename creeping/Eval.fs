module Eval

open Types
open Syntax
open Values

let rec eval (expr : typexpr) (env : value env) =
   
    match expr with
        | ConstN   -> ValL []
        | ConstI i -> ValI i
        | ConstB b -> ValB b
        | ConstR r -> ValR r
        | ConstC c -> ValC c
        | ConstS s -> ValS s
        | Var x -> lookup env x
        | Sel(i, xs) ->

            match eval xs env with
                | ValT xs ->  List.item (i - 1) xs
                | _ -> failwith "Run-time error SEL\n"

        | Tup xs -> ValT (List.map (fun expr -> eval expr env) xs)
        | List [] -> ValL []
        | List xs -> ValL (List.map (fun expr -> eval expr env) xs)
        | Cons(e, ConstN) -> ValL [eval e env]
        | Cons(e1, e2) -> 

            let v1 = eval e1 env
            let v2 = eval e2 env

            match (v1, v2) with
                | (v, ValL vs) -> ValL (v::vs)
                | _ -> failwith "Run-time error CONS\n"

        | Let(xdefs, body) -> 

            let x, xdefs = List.unzip xdefs
            let xvals = List.map (fun x -> eval x env) xdefs
            let extendedEvn = (List.zip x xvals) @ env

            eval body extendedEvn

        | Prim(op, e1, e2) ->

            let ve1 = eval e1 env
            let ve2 = eval e2 env

            match (op, ve1, ve2) with

                | ("+", ValI i1, ValI i2) -> ValI(i1 + i2)
                | ("-", ValI i1, ValI i2) -> ValI(i1 - i2)
                | ("*", ValI i1, ValI i2) -> ValI(i1 * i2)
                | ("/", ValI i1, ValI i2) -> ValI(i1 / i2)
                | ("%", ValI i1, ValI i2) -> ValI(i1 % i2)

                | ("+", ValR r1, ValR r2) -> ValR(r1 + r2)
                | ("-", ValR r1, ValR r2) -> ValR(r1 - r2)
                | ("*", ValR r1, ValR r2) -> ValR(r1 * r2)
                | ("/", ValR r1, ValR r2) -> ValR(r1 / r2)

                | ("+", ValS s1, ValS s2) -> ValS(s1 + s2)
                | ("^", ValS s1, ValS s2) -> ValS(sprintf "%s%s" s1 s2)

                | ("*", ValS s, ValI i) -> ValS(String.replicate i s)
                | ("*", ValI i, ValS s) -> ValS(String.replicate i s)

                | ("&&", ValB b1, ValB b2) -> ValB(b1 && b2)
                | ("||", ValB b1, ValB b2) -> ValB(b1 || b2)

                | ("<", ValI i1, ValI i2)  -> ValB(i1 < i2)
                | ("<=", ValI i1, ValI i2) -> ValB(i1 <= i2)
                | (">", ValI i1, ValI i2)  -> ValB(i1 > i2)
                | (">=", ValI i1, ValI i2) -> ValB(i1 >= i2)
                | ("=", ValI i1, ValI i2)  -> ValB(i1 = i2)
                | ("<>", ValI i1, ValI i2) -> ValB(i1 <> i2)

                | ("<", ValR r1, ValR r2)  -> ValB(r1 < r2)
                | ("<=", ValR r1, ValR r2) -> ValB(r1 <= r2)
                | (">", ValR r1, ValR r2)  -> ValB(r1 > r2)
                | (">=", ValR r1, ValR r2) -> ValB(r1 >= r2)
                | ("=", ValR r1, ValR r2)  -> ValB(r1 = r2)
                | ("<>", ValR r1, ValR r2) -> ValB(r1 <> r2)

                | ("<", ValC s1, ValC s2)  -> ValB(s1 < s2)
                | ("<=", ValC s1, ValC s2) -> ValB(s1 <= s2)
                | (">", ValC s1, ValC s2)  -> ValB(s1 > s2)
                | (">=", ValC s1, ValC s2) -> ValB(s1 >= s2)
                | ("=", ValC s1, ValC s2)  -> ValB(s1 = s2)
                | ("<>", ValC s1, ValC s2) -> ValB(s1 <> s2)

                | ("<", ValS s1, ValS s2)  -> ValB(s1 < s2)
                | ("<=", ValS s1, ValS s2) -> ValB(s1 <= s2)
                | (">", ValS s1, ValS s2)  -> ValB(s1 > s2)
                | (">=", ValS s1, ValS s2) -> ValB(s1 >= s2)
                | ("=", ValS s1, ValS s2)  -> ValB(s1 = s2)
                | ("<>", ValS s1, ValS s2) -> ValB(s1 <> s2)

                | _ -> failwith "Run-time error PRIM\n"

        | If(e1, e2, e3) ->

            match eval e1 env with
                | ValB true  -> eval e2 env
                | ValB false -> eval e3 env
                | _ -> failwith "Run-time error IF\n"

        | Letfun(fname, xs, _, fbody, letbody) ->

            let xdefs, _ = List.unzip xs
            let bodyenv = (fname, Closure(fname, xdefs, fbody, env)) :: env

            eval letbody bodyenv

        | Call(Var fname, args) ->

            let closure = lookup env fname

            match closure with
                | Closure(fname, xs, fbody, fprevenv) ->

                    let xsvals = List.map (fun x -> eval x env) args
                    let zippedValues = List.zip xs xsvals
                    let fbodyenv = zippedValues @ ((fname, closure) :: fprevenv)
                        
                    eval fbody fbodyenv

                | _ -> failwith "Run-time error CALL\n"

        | Call _ -> failwith "Not a first-order function\n"
        | Match(e0, e1, (h, t, e2)) ->

            match eval e0 env with
                | ValL [] -> eval e1 env
                | ValL(x::xs) -> 

                    let extendedEnv = (h, x) :: (t, ValL xs) :: env

                    eval e2 extendedEnv

                | _ -> failwith "Run-time error MATCH\n"

        | Range(low, step, high) ->

            let getRange l s h = 

                let rec getRangeAux l acc = 
                    if l > h then acc 
                    else getRangeAux (l + s) (l::acc)

                getRangeAux l []

            let vl = eval low env
            let vh = eval high env

            match (low, step, high) with
                | (low, None, high) -> 

                    match (vl, vh) with
                        | (ValI il, ValI ih) -> 

                            let nativeRange = getRange il 1 ih |> List.rev
                            
                            List.map (fun x -> ValI x) nativeRange |> ValL

                        | _ -> failwith "Run-time error RANGE None\n"

                | (low, Some step, high) ->

                    let vs = eval step env

                    match (vl, vs, vh) with
                        | (ValI il, ValI is, ValI ih) -> 

                            let nativeRange = getRange il is ih |> List.rev
                            
                            List.map (fun x -> ValI x) nativeRange |> ValL

                        | _ -> failwith "Run-time error RANGE Some\n"

                | _ -> failwith "Run-time error RANGE\n"
  