namespace Tiger

open FParsec

type TigerCompiler () =
    let mutable dumpSyntaxTree = true
    
    let translate syntax =
        
        Escape.FindEscape syntax
        if dumpSyntaxTree then 
            printfn "*** Syntax start"
            PrintAbsyn.print syntax
            printfn "***  Syntax End"

        let ir = 
            Semant.transProg syntax
        ()

    
    member public s.DumpSyntaxTree 
        with get() =
            dumpSyntaxTree
        and set value =
            dumpSyntaxTree <- value

    // Primarily for testing
    member s.CompileString str =
        let syntax = match runParserOnString Parser.prog () "" str with
                     | Success (result,_,_) -> result
                     | Failure (error, _,_) -> 
                        printfn "Parse Error: %s" error
                        raise (TigerExceptions.ParseError error)        

        translate syntax

    member s.CompileFile path =
        let syntax = match runParserOnFile Parser.prog () path System.Text.Encoding.UTF8 with
                     | Success (result,_,_) -> result
                     | Failure (error, _,_) -> 
                        printfn "Parse Error: %s" error
                        raise (TigerExceptions.ParseError error)

        translate syntax