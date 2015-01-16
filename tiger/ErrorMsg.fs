module ErrorMsg

type ErrorMsg (filename : string) =
   
    member this.error position message =
        () // Hell if I know -- depends on how positions are stored in fsyacc

    member this.impossible msg =
        failwithf "Compiler Error: %s\n" msg
   