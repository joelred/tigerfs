module SymbolNS

open System.Collections.Generic

type Symbol( name: string, idCode: int ) = 

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

     member x.Name = name

     member private x.Id = idCode

     override x.Equals(yobj) =
        match yobj with
        | :? Symbol as y -> (x.Id = y.Id)
        | _ -> false

    override x.GetHashCode () =
        x.Id

    override x.ToString () =
        x.Name

    interface System.IComparable with
        override x.CompareTo(yobj) =
            match yobj with
            | :? Symbol as y -> x.Id - y.Id
            | _ -> invalidArg "yobj" "Invalid arg in Symbol.CompareTo"



type Table<'a> (map : Map<Symbol, 'a>) =
    
    static member empty = Table(Map.empty)

    member t.enter k v =
         Table(Map.add k v map)
    
    member t.look k =
        Map.tryFind k map
