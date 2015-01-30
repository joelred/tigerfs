module internal Tiger.Temps

type temp = int

type Temp private (index : temp) =
    
    static let mutable nextTemp = 100
    
    static member NewTemp () =
        nextTemp <- nextTemp + 1
        Temp (nextTemp)
       
    member private x.Id = index

    override x.ToString () =
        sprintf "t%d" index
    
     override x.Equals(yobj) =
        match yobj with
        | :? Temp as y -> (x.Id = y.Id)
        | _ -> false

    override x.GetHashCode () =
        x.Id

    interface System.IComparable with
        override x.CompareTo(yobj) =
            match yobj with
            | :? Temp as y -> x.Id - y.Id
            | _ -> invalidArg "yobj" "Invalid arg in Symbol.CompareTo"
    

type Label (name : string) =
    
    static let mutable nextLabel = 100
    new () = 
        let labelName = sprintf "l%d" nextLabel
        nextLabel <- nextLabel + 1
        Label (labelName)

    override x.ToString () =
        name

type internal TempTable<'a> (map : Map<Temp, 'a>) =
    
    static member empty = TempTable(Map.empty)

    member t.Add k v =
         TempTable(Map.add k v map)
    
    member t.TryFind k =
        Map.tryFind k map
    
    member t.ForEach f =
        Map.iter f map  