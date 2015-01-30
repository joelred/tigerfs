module internal Tiger.Types 

type unique = int

let mutable nextVal = 0
let nextUnique () = 
    nextVal <- nextVal + 1
    nextVal

type Alias =
    { 
        Name : Symbols.Symbol;
        mutable Type: Type option }

and Type =
    | Array of Type * unique
    | Record of (Symbols.Symbol * Type) list * unique
    | Nil
    | Int
    | String
    | Name of Alias
    | Unit    
    | Top

let rec bareType type' = 
    match type' with
    | Name alias -> 
        match alias.Type with 
        | Some t -> bareType t
        | None -> type'
    | _ ->
        type'

let typesMatch l namedR =
    let r = bareType namedR

    match bareType l with
    | Array (_, lu) ->
        match r with
        | Array (_, ru) -> lu = ru 
        | Top -> true
        |_ -> false
    | Record (_, lu) ->
        match r with
        | Nil -> true        
        | Record (_, ru) -> lu = ru
        | Top -> true
        | _ -> false
    | Nil ->
        match r with         
        | Nil -> false        
        | Record _ -> true
        | Top -> true
        | _ -> false
    | Int ->
        match r with
        | Int -> true
        | Top -> true
        | _ -> false
    | String ->
        match r with
        | String -> true
        | Top -> true
        | _ -> false
    | Name _ -> false
    | Unit -> 
        match r with 
        | Unit -> true
        | Top -> true
        | _ -> false
    | Top -> true

let typesMismatch typ1 typ2 =
    not (typesMatch typ1 typ2)
    
