module Types

type unique = int

let mutable nextVal = 0
let nextUnique = 
    nextVal <- nextVal + 1
    nextVal

type Alias =
    { 
        Name : SymbolNS.Symbol;
        mutable Type: Type option }

and Type =
    | Array of Type * unique
    | Record of (SymbolNS.Symbol * Type) list * unique
    | Nil
    | Int
    | String
    | Name of Alias
    | Unit    
    | Error
