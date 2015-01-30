namespace Tiger

open FParsec

type TigerCompiler () =
    let mutable dumpSyntaxTree = false
    let mutable dumpDesugaredTree = false
    let mutable dumpHIR = false

    let dumpSyntax doIt tree =
        if doIt then 
            printfn "*** Syntax start"
            PrintAbsyn.print tree
            printfn "***  Syntax End"


    let translate syntax =
        syntax 
            |> Escape.FindEscape
            |> dumpSyntax dumpSyntaxTree
            
       
        ()
    
    member public s.DumpSyntaxTree 
        with get() =
            dumpSyntaxTree
        and set value =
            dumpSyntaxTree <- value

    member public s.DumpDesugaredTree 
        with get() =
            dumpDesugaredTree
        and set value =
            dumpDesugaredTree <- value

    member public s.DumpHIRTree 
        with get() =
            dumpDesugaredTree
        and set value =
            dumpDesugaredTree <- value


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