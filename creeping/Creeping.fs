(* fsharpi -r FSharp.PowerPack.dll Types.fs Syntax.fs Parser.fs Lexer.fs Values.fs Typechecker.fs Eval.fs Creeping.fs *)

module Creeping

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing

open Syntax
open Types
open Values
open Typechecker
open Eval

(* Plain parsing from a string, with poor error reporting *)

let exprFromString (str : string) : typexpr =
    let lexbuf = Lexing.LexBuffer<char>.FromString(str)
    try 
        Parser.Main Lexer.Token lexbuf
    with 
        | exn -> 
            let pos = lexbuf.EndPos in
                failwithf "%s near line %d, column %d\n" 
                    (exn.Message) (pos.Line + 1) pos.Column

(* Plain parsing from a file, with poor error reporting *)

let exprFromFile (filename : string) : typexpr =
    use reader = new StreamReader(filename)
    let lexbuf = Lexing.LexBuffer<char>.FromTextReader reader
    try 
          Parser.Main Lexer.Token lexbuf
    with 
        | exn -> 
            let pos = lexbuf.EndPos in 
                failwithf "%s in file %s near line %d, column %d\n" 
                    (exn.Message) filename (pos.Line + 1) pos.Column

let evalFromString (str : string) =

    let expr = exprFromString str
    let typechecked = typecheck expr []
    let value = eval expr []

    let stype = typechecked |> stringOfType
    let svalue = value |> stringOfValue

    sprintf "value : %s = %s" stype svalue

let evalFromFile (str : string) =

    let expr = exprFromFile str
    let typechecked = typecheck expr []
    let value = eval expr []

    let stype = typechecked |> stringOfType
    let svalue = value |> stringOfValue

    sprintf "value : %s = %s" stype svalue

let rec REPL () =

    printf ">> ";
    let program = System.Console.ReadLine();
    let pragma = program.[0]

    match pragma with
        | '#' ->
            let command = program.[1]

            match command with
                | 'q' | 'Q' -> 
                    printf "%s\n" "- Exit..."; 
                    ()

                | 'l' | 'L' ->
                    let length = String.length program
                    let filename = program.[4..length - 2]

                    let response = evalFromFile filename;
                    printf "%s\n" response;
                    REPL();

                | _ -> REPL();

        | _ -> 

            let response = evalFromString program;
            printf "%s\n" response;
            REPL();

let run () =

    printf "Interactive Creeping REPL\n\n"
    REPL ()
