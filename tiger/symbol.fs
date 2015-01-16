module SymbolNS

open System.Collections.Generic

type Symbol( name: string, code: int ) = 

    static let mutable nextValue = 0
    static let mutable dictionary = Map.empty;
        
    static member Symbol name = 
        match dictionary.TryFind(name) with
        | Some sym -> sym
        | None ->
            let i = nextValue
            let sym = Symbol(name, i)
            nextValue <- nextValue + 1
            dictionary <- Map.add name sym dictionary
            sym

     member this.Name = name

