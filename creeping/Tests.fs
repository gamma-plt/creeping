(* fsharpi -r FSharp.PowerPack.dll Types.fs Syntax.fs Parser.fs Lexer.fs Values.fs Typechecker.fs Eval.fs Creeping.fs Tests.fs *)

module Tests

open Types
open Syntax
open Values
open Eval
open Typechecker
open Creeping

let frontEndTest =

    let testCase1 = "if (a + 5 = b) then if #1(true, a, b) then c else a else #2((1, 2, 3), [1], (1, 2))"

    let testCase2 = "if (a + 5 = b) then if #1(true, a, b) then c else a else #2((1, 2, 3), [1], (1, 2), 2::3::[])"

    let testCase3 = "let a, b, c = 1, 2, 3 in if (a + 5 = b) then if #1(true, a, b) then c else a else #2((1, 2, 3), [1], (1, 2), 2::3::[]) end"

    let testCase4 = "let max (a : int) (b : int) : int = if a > b then a else b in max end"

    let testCase5 = "let max (a : int) (b : [int]) : int = if a > b then a else b in max end"

    let testCase6 = "let max (a : int) (b : (int * int)) : int = if a > b then a else b in max end"

    let testCase7 = "let max (a : int) (b : (int * int)) : (int * int) = if a > b then a else b in max end"

    let testCase8 = "let max (a : int) (b : (int * int)) : (int * int) = if a > b then a else b in max (if a > b then 3 else 4) 5 end"

    let testCase9 = "let max (a : int) (b : (int * int)) : [int] = [1..b] in max end"

    let testCase10 = "let max (a : int) (b : (int * int)) : [int] = [1, 2..b] in max end"

    let testCase11 = "let max (a : int) (b : (int * int)) : [int] = [1, 2 .. b + 6] in max end"

    let testCase12 = "let sum (xs : [int]) : int = match xs with [] -> 0 | x::xs -> if multiple x then x + (sum xs) else sum xs in sum [1..b+ (b - 8)] (1, 2, 3) end"

    let testCase13 = "let x = (b - 6 * (7 > 9)) in x end"

    let testCase14 = "let x = (b - 6 * (7 / 9)) in x end"

    let testCase15 = "let x = (b + 6 * (7 - 9)) in x end"

    let testCase16 = "let x = (b + 6 - (7 + 9)) in x end"

    let testCase17 = "let x = b + 6 - 7 + 9 in x end"

    let testCase18 = "let x = b + 6 - 7 in x end"

    let testCase19 = "let x = b + 6 - (6) in x end"

    let testCase20 = "let max (a : int) (b : (int * int)) : [int] = [1, 2 .. b + 6 - (6)] in max end"

    let testCase21 = "let max (a : int) (b : (int * int)) : [int] = [1 .. b + 6 - 6] in max end"

    let testCase22 = "let max (a : int) (b : (int * int)) : [int] = [1, 2 .. b + 6 - 6] in max end"

    let testCase23 = "let sum (xs : [int]) : int = match xs with [] -> 0 | x::xs -> if multiple x then x + (sum xs) else sum xs in sum [1,2,3] end"

    let testCase24 = "let sum (xs : [int]) : int = match xs with [] -> 0 | x::xs -> if multiple x then x + (sum xs) else sum xs in sum [1..b+ (b - 8)] end"

    let testCase25 = "let sum (xs : [int]) : int = match xs with [] -> 0 | x::xs -> if multiple x then x + (sum xs) else sum xs in x end"

    let testCases = [
        testCase1;
        testCase2;
        testCase3;
        testCase4;
        testCase5;
        testCase6;
        testCase7;
        testCase8;
        testCase9;
        testCase10;
        testCase11;
        testCase12;
        testCase13;
        testCase14;
        testCase15;
        testCase16;
        testCase17;
        testCase18;
        testCase19;
        testCase20;
        testCase21;
        testCase22;
        testCase23;
        testCase24;
    ]

    List.map exprFromString testCases

let backEndTest =
    
    (* 
    let fact (n : int) : int =
        let aux (n : int) (acc : int) : int = 
            if n = 0 then acc
            else aux (n - 1) (n * acc)
        in aux n 1 end
    in fact 5 end *)

    let testCase1 =
        Letfun(
            "fact",
            [("n", TypeI)],
            TypeI,
            Letfun(
                "aux",
                [("n", TypeI); ("acc", TypeI)],
                TypeI,
                If(
                    Prim(
                        "=",
                        Var "n", 
                        ConstI 0),
                    Var "acc",
                    Call(
                        Var "aux",
                        [Prim("-", Var "n", ConstI 1); Prim("*", Var "n", Var "acc")])),
                Call(
                    Var "aux",
                    [Var "n"; ConstI 1])),
            Call(
                Var "fact",
                [ConstI 5]))

    (* 
    let len (xs : [int]) : int = 
        match xs with
            [] -> 0
            | h::t -> 1 + len t
        in len [1, 2, 3, 4, 5, 6] end
    end *)

    let testCase2 = 
        Letfun(
            "len",
            [("xs", TypeL TypeI)],
            TypeI,
            Match(
                Var "xs",
                ConstI 0,
                (
                    "h",
                    "t",
                    Prim(
                        "+", 
                        ConstI 1, 
                        Call(
                            Var "len",
                            [Var "t"])))),
            Call(
                Var "len",
                [Cons(ConstI 1,
                    Cons(ConstI 2,
                        Cons(ConstI 3,
                            Cons(ConstI 4,
                                Cons(ConstI 5,
                                    Cons(ConstI 6, ConstN))))))]))

    (*
    let multiple (n : int) : bool = (n % 3 = 0) || (n % 5 = 0) in
        let sum (xs : [int]) : int =
            match xs with
                [] -> 0
                | x::xs -> 
                    if multiple x then x + (sum xs)
                    else sum xs
        in sum [1..(1000 - 1)] end
    end *)

    let testCase3 = 
        Letfun(
            "multiple",
            [("n", TypeI)],
            TypeB,
            Prim(
                "||",
                Prim(
                    "=",
                    Prim(
                        "%",
                        Var "n",
                        ConstI 3),
                    ConstI 0),
                Prim(
                    "=",
                    Prim(
                        "%",
                        Var "n",
                        ConstI 5),
                    ConstI 0)),
            Letfun(
                "sum",
                [("xs", TypeL TypeI)],
                TypeI,
                Match(
                    Var "xs",
                    ConstI 0,
                    (
                        "x",
                        "xs",
                        If(
                            Call(
                                Var "multiple",
                                [Var "x"]),
                            Prim(
                                "+",
                                Var "x", 
                                Call(
                                    Var "sum",
                                    [Var "xs"])),
                            Call(
                                    Var "sum",
                                    [Var "xs"])))),
                Call(
                    Var "sum",
                    [Range(
                        ConstI 1,
                        None,
                        Prim("-", ConstI 1000, ConstI 1))])))

    (*
    let max x y = if x > y then x else y in
        let max3 xyz = 
            let x, y, z = #1(xyz), #2(xyz), #3(xyz) in

                max x (max y z) end

        in max3 ('a', 'b', 'c') end
    end *)

    let testCase4 = 
        Letfun(
            "max",
            [("x", TypeC); ("y", TypeC)],
            TypeC,
            If(
                Prim(">", Var "x", Var "y"),
                Var "x",
                Var "y"),
            Letfun(
                "max3",
                [("xyz", TypeT [TypeC; TypeC; TypeC])],
                TypeC,
                Let(
                    [("x", Sel(1, Var "xyz")); ("y", Sel(2, Var "xyz")); ("z", Sel(3, Var "xyz"))],
                    Call(
                        Var "max",
                        [
                            Var "x";
                            Call(Var "max", [Var "y"; Var "z"])
                        ])),
                Call(
                    Var "max3", 
                    [Tup [ConstC 'a'; ConstC 'b'; ConstC 'c']])))


    (*
    let even n = n % 2 = 0 in
        let evens xs =
            match xs with
                [] -> []
                | x::xs -> 
                    if even x then (x::evens xs)
                    else evens xs
        in evens [1..1000] end
    end *)

    let testCase5 =
        Letfun(
            "even",
            [("n", TypeI)],
            TypeB,
            Prim("=", Prim("%", Var "n", ConstI 2), ConstI 0),
            Letfun(
                "evens",
                [("xs", TypeL TypeI)],
                TypeL TypeI,
                Match(
                    Var "xs",
                    ConstN,
                    (
                        "x",
                        "xs",
                        If(
                            Call(
                                Var "even",
                                [Var "x"]),
                            Cons(
                                Var "x",
                                Call(Var "evens", [Var "xs"])),
                            Call(Var "evens", [Var "xs"])))),
                Call(
                    Var "evens", 
                    [Range(ConstI 1, None, ConstI 10)])))

    (* let x = [1,2,3,4,5,6] in x end *)

    let testCase6 = 
        Let(
            [("x",
                Cons(
                    ConstI 1, 
                    Cons(
                        ConstI 2, 
                        Cons(ConstI 3, 
                            Cons(
                                ConstI 4, 
                                Cons(
                                    ConstI 5,
                                    Cons(
                                        ConstI 6, 
                                        ConstN)))))))],
         Var "x")

    (*
    let even n = n % 2 = 0 in
        let evens xs =
            match xs with
                [] -> []
                | x::xs -> 
                    if even x then (x::evens xs)
                    else evens xs
        in evens [] end
    end *)

    let testCase7 =
        Letfun(
            "even",
            [("n", TypeI)],
            TypeB,
            Prim("=", Prim("%", Var "n", ConstI 2), ConstI 0),
            Letfun(
                "evens",
                [("xs", TypeL TypeI)],
                TypeL TypeI,
                Match(
                    Var "xs",
                    ConstN,
                    (
                        "x",
                        "xs",
                        If(
                            Call(
                                Var "even",
                                [Var "x"]),
                            Cons(
                                Var "x",
                                Call(Var "evens", [Var "xs"])),
                            Call(Var "evens", [Var "xs"])))),
                Call(
                    Var "evens", 
                    [ConstN])))

    (*
    let even n = n % 2 = 0 in
        let evens xs =
            match xs with
                [] -> []
                | x::xs -> 
                    if even x then (x::evens xs)
                    else evens xs
        in [] end
    end *)

    let testCase8 =
        Letfun(
            "even",
            [("n", TypeI)],
            TypeB,
            Prim("=", Prim("%", Var "n", ConstI 2), ConstI 0),
            Letfun(
                "evens",
                [("xs", TypeL TypeI)],
                TypeL TypeI,
                Match(
                    Var "xs",
                    ConstN,
                    (
                        "x",
                        "xs",
                        If(
                            Call(
                                Var "even",
                                [Var "x"]),
                            Cons(
                                Var "x",
                                Call(Var "evens", [Var "xs"])),
                            Call(Var "evens", [Var "xs"])))),
                ConstN))

    let testCases = [
        testCase1;
        testCase2;
        testCase3;
        testCase4;
        testCase5;
        testCase6;
        testCase7;
        testCase8
    ]

    let testCasesTypes = List.map (fun x -> typecheck x []) testCases
    let testCasesValues = List.map (fun x -> eval x []) testCases

    let types = List.map stringOfType testCasesTypes
    let values = List.map stringOfValue testCasesValues

    List.zip types values