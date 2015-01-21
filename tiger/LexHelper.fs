module LexHelper

open Microsoft.FSharp.Text.Lexing
open SymbolNS

let newline (lexbuf: LexBuffer<_>) = 
  lexbuf.EndPos <- lexbuf.EndPos.NextLine

let ids = [ ("var", Parser.VAR);
            ("nil", Parser.NIL);
            ("function", Parser.FUNCTION);
            ("let", Parser.LET);
            ("in", Parser.IN);
            ("end", Parser.END);
            ("of", Parser.OF);
            ("if", Parser.IF);
            ("then", Parser.THEN);
            ("else", Parser.ELSE);
            ("while", Parser.WHILE);
            ("do", Parser.DO);
            ("for", Parser.FOR);
            ("to", Parser.TO);
            ("break", Parser.BREAK); ]

let idsMap = Map.ofList ids

let lexeme lexbuf =
    LexBuffer<_>.LexemeString lexbuf

let ident lexbuf =
    let id = lexeme lexbuf
        
    if Map.containsKey id idsMap 
        then Map.find id idsMap
        else Parser.ID
             <| Symbol.Symbol (id)
   

let createString lst =
    lst |> List.rev |> String.concat ""


