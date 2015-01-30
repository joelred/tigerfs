module internal Tiger.Frames

open Temps

type Access =     
    | InFrame of int
    | InReg of Temp

type Frame = 
    abstract member Name : Label with get
    abstract member Formals : Access list with get
    abstract member AllocLocal : bool -> Access


type X64Frame (name: Label, formalEscapes : bool list) =
    let mutable nextSlot = 0

    let getNextLocal () = 
        let loc = nextSlot
        nextSlot <- nextSlot - 8;
        loc

    let formals = 
        List.map
            (fun formalEscapes ->
                if formalEscapes then
                    InFrame (getNextLocal() )
                else
                    InReg (Temp.NewTemp() ))
            formalEscapes

    interface Frame with
        member x.Name = name
        
        member x.Formals = formals
        
        member x.AllocLocal escapes = 
            if escapes then
                InFrame (getNextLocal())
            else
                InReg ( Temp.NewTemp() )


let newFrame (name, formals) =
    new X64Frame(name, formals)