module ErrorMsgNS

type ErrorMsg () =
   
    static let maxErrors = 50;

    static let mutable errorCount = 0
    static member HasErrors =
        errorCount > 0

    static member PrintCount =
        printfn "%d errors found" errorCount

    static member Error (pos: FParsec.Position) message =
        errorCount <- errorCount + 1;
        printfn "%s(%d, %d): Error %s" pos.StreamName pos.Line pos.Column message

        if errorCount > maxErrors then
            ErrorMsg.PrintCount
            raise (Exceptions.SemanticError "Too many errors")
    
    static member Impossible msg =
        failwithf "Compiler Error: %s\n" msg
   
    static member reset =         
        errorCount <- 0