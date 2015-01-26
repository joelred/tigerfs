module Semant

open Absyn
open Environment
open ErrorMsgNS
open Types

type ExpTy = Translate.Exp * Types.Type

let bareType type' = 
    match type' with
    | Name alias -> 
        match alias.Type with 
        | Some t -> t
        | None -> type'
    | _ ->
        type'

let typesMatch l namedR =
    let r = bareType namedR

    match bareType l with
    | Array (_, lu) ->
        match r with
        | Array (_, ru) -> lu = ru 
        | Error -> true
        |_ -> false
    | Record (_, lu) ->
        match r with
        | Nil -> true        
        | Record (_, ru) -> lu = ru
        | Error -> true
        | _ -> false
    | Nil ->
        match r with         
        | Nil -> true        
        | Record _ -> true
        | Error -> true
        | _ -> false
    | Int ->
        match r with
        | Int -> true
        | Error -> true
        | _ -> false
    | String ->
        match r with
        | String -> true
        | Error -> true
        | _ -> false
    | Name _ -> false
    | Unit -> 
        match r with 
        | Unit -> true
        | Error -> true
        | _ -> false
    | Error -> true

let typesMismatch typ1 typ2 =
    not (typesMatch typ1 typ2)

let getType name pos (tenv: TypeEnv) = 
    let type' = tenv.TryFind name
    match type' with
    | Some t ->
        t
    | None -> 
        ErrorMsg.Error pos (sprintf "Unknown type: %A" name)
        Error

let hasDuplicateNames decs errorMsg=         
        let (hasDuplicate, _) = 
            List.fold 
                (fun (hasDuplicate, nameSet) (name, pos) -> 
                    if Set.contains name nameSet then
                        ErrorMsg.Error pos (sprintf errorMsg name)
                        (true, nameSet)
                    else
                        (hasDuplicate, Set.add name nameSet))
                (false, Set.empty)
                decs
        hasDuplicate

let translateType type' (tenv : TypeEnv) =
    match type' with 
    | NameType (sym, pos) -> getType sym pos tenv
    | RecordType fieldList ->
        let unique = Types.nextUnique
        let typeList =
            //TODO: Make sure field names are unique

            List.map 
                (fun (field : Field) -> 
                    let fieldType = getType field.Type field.Position tenv
                    (field.Name, fieldType))                    
                fieldList
        Record (typeList, unique)
    | ArrayType (baseTypeName, pos) ->
        let uniquifier = Types.nextUnique
        let baseType = getType baseTypeName pos tenv
        Array(baseType, uniquifier)  
                           
let rec translateDec dec (env : Env) : Env =
    let (tenv, venv) = env

    let rec translateFunctionDecs (decs : FunctionDecType list)  =
        //First make sure decs doesn't contain any duplicate names
        let namepos = List.map (fun (dec : FunctionDecType) -> (dec.Name, dec.Position)) decs
        if not <| hasDuplicateNames namepos "Duplicate function name %A found in group" then

            /// Get just the function signature without worrying about the body
            let getSignature venv (dec:FunctionDecType) = 
                let resultType =
                    match dec.Result with
                    | Some (typeName, pos) -> getType typeName pos tenv                
                    | None -> Unit
            
                let paramTypes = List.map 
                                    (fun (param:Field) -> getType param.Name param.Position tenv)
                                    dec.Params
             
                { Formals = paramTypes; Result = resultType; }
       
            /// Updated environment with all the function signatures
            let functionsVarEnv = 
                List.fold  
                    (fun (venv:VarEnv) (dec:FunctionDecType) ->
                            venv.Add dec.Name (FunEntry (getSignature venv dec)))
                    venv
                    decs
                    
            /// Translate a function body
            let translateFunction (func:FunctionDecType) =
                // make sure that the function parameter names are all unique
                let namepos = List.map 
                                (fun (field:Field) -> (field.Name, field.Position)) 
                                func.Params
                ignore <| hasDuplicateNames namepos "Duplicate parameter name %A found" 

                // Get the current function's signature
                let funEntry = 
                    match functionsVarEnv.TryFind func.Name with
                    | Some (FunEntry functionHeader) -> functionHeader
                    | _ -> 
                        // We just inserted these in functionsEnv, so it better be there
                        ErrorMsg.Impossible "Compiler error: Unknown function %A" func.Name
                // Insert the parameters into the namespace
                let functionVenv =
                    List.fold2 
                        (fun (venv:VarEnv) (field: Field) type' -> 
                            venv.Add field.Name (VarEntry {Type = type'; CanAssign = true}) )
                        functionsVarEnv
                        func.Params
                        funEntry.Formals
            
                let (bodyExp, bodyType) = translateExp func.Body (tenv, functionVenv) None

                if typesMismatch bodyType funEntry.Result then
                    ErrorMsg.Error func.Position "Body type doesn't match declared type"
            
            List.iter translateFunction decs
            (tenv,functionsVarEnv)
        else
            (tenv,venv)

    let translateVarDec dec =
        let (initExp, initType) = translateExp dec.Init env None
        match dec.Type with
        | Some (declaredTypeName,pos) ->
            let declaredType = tenv.TryFind declaredTypeName
            match declaredType with
            | Some type' ->
                if typesMismatch type' initType then
                    ErrorMsg.Error dec.Position "Declared type must match initializer"
                    (tenv, venv.Add dec.Name (VarEntry { Type = Error; CanAssign = true }))
                else
                    (tenv, venv.Add dec.Name (VarEntry { Type = type'; CanAssign = true }))                
            | None ->
                ErrorMsg.Error dec.Position (sprintf "Declared type %A does not exist" declaredTypeName)
                (tenv, venv.Add dec.Name (VarEntry { Type = Error; CanAssign = true }))   
        | None -> 
            (tenv, venv.Add dec.Name (VarEntry { Type = initType; CanAssign = true }))   

    let translateTypeDecs decs =
        //First make sure decs doesn't contain any duplicate names
        let namepos = List.map (fun (dec : TypeDecType) -> (dec.Name, dec.Position)) decs
        if not <| hasDuplicateNames namepos "Duplicate type name %A found in group" then
                 
            // Add all the type names without binding the actual types
            let addType (tenv:TypeEnv) (dec:TypeDecType) =
                tenv.Add 
                    dec.Name 
                    (Name {Name=dec.Name; Type = None})

            // Fill out the actual types
            let augmentedTypeEnv = List.fold addType tenv decs
        
            let assignType (dec : TypeDecType) =                
                let alias = 
                    match augmentedTypeEnv.TryFind dec.Name with                
                    | Some (Name a)-> a
                    | _ -> ErrorMsg.Impossible "Compiler Error: Unknown type %A after insertion"

                let type' = translateType dec.Type augmentedTypeEnv

                alias.Type <- Some type'

            List.iter assignType decs

            (augmentedTypeEnv, venv)
        else
            env

    match dec with 
    | FunctionDec decs ->
        translateFunctionDecs decs       
    | VarDec var ->
        translateVarDec var
    | TypeDec decs ->
        translateTypeDecs decs

and translateDecs decs env =
    List.fold 
        (fun env dec -> translateDec dec env)
        env
        decs

and translateVar var (env : Env) =
    let (typeEnv, varEnv) = env
    
    match var with
    | SimpleVar (sym, pos) -> 
        let binding = varEnv.TryFind sym
        match binding with 
        | Some (VarEntry varType) ->
            ((), varType.Type, varType.CanAssign)
        | Some (FunEntry _) ->
            ErrorMsg.Error pos (sprintf "%A is a function" sym)
            ((), Error, false)          
        | None ->
            ErrorMsg.Error pos (sprintf "%A is not defined" sym)
            ((), Error, true)
    
    | FieldVar (var, fieldName, pos) ->
        let (_, type', canAssign) = translateVar var env
        match bareType type' with
        | Record (fieldList, _) ->
            match List.tryFind (fun (sym, type') -> fieldName = sym) fieldList with
            | Some (sym, type') -> ((), type', true)
            | None -> 
                ErrorMsg.Error pos (sprintf "%A is not a field in this record" fieldName)
                ((), Error, true)
        | Error -> ((), Error, true)
        | _ -> 
            ErrorMsg.Error pos "Attempt to assign field in non-record type"
            ((), Error, true)

    | SubscriptVar (var, subscript, pos) ->
        let (_, type', canAssign) = translateVar var env
        match bareType type' with
        | Array (type', _) ->
            ((), type', true)
        | Error -> 
            ((), Error, true)
        | _ -> 
            ErrorMsg.Error pos "Attempt to subscript in non-array type"
            ((), Error, true)

and translateExp exp (env : Env) (breakLabel : bool option) : ExpTy =
    let (typeEnv, varEnv) = env
    
    let translateCall call (env : Env) =
        let (typeEnv, varEnv) = env

        match varEnv.TryFind call.Func with
        | Some (FunEntry f) -> 
            if( f.Formals.Length = call.Args.Length ) then
                List.iter2 
                    (fun expectedType receivedExp ->
                        let (_, receivedType) = translateExp receivedExp env None
                        if typesMismatch receivedType expectedType then
                            ErrorMsg.Error call.Position (sprintf "Type mismatch in arguments to %A" call.Func) )
                    f.Formals
                    call.Args
            else 
                ErrorMsg.Error call.Position (sprintf "Expected %d args, received %d" f.Formals.Length call.Args.Length)                

            ((), f.Result)

        | _ -> 
            ErrorMsg.Error call.Position (sprintf "Function expected at %A" call.Func)
            ((), Error)

    let translateOperator op (env : Env) =        
        let (leftExp, leftType) = translateExp op.Left env breakLabel
        let (rightExp, rightType) = translateExp op.Right env breakLabel
      
        match bareType leftType with
        | Error -> 
            ((), Error)
        | Record (_,lu) ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Record(_,ru ) ->
                    if lu <> ru 
                    then ErrorMsg.Error op.Position "Records not of the same type"                
                | Nil 
                | Error ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"
                
                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on record"
                ((), Error)
        | Array (_,lu) ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Array(_,ru ) ->
                    if lu <> ru 
                    then ErrorMsg.Error op.Position "Arrays not of the same type"                                           
                | Error -> 
                    ()                
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"            
            
                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on array"
                ((), Error)
        | Nil ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Record _                               
                | Nil 
                | Error ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"

                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on record"
                ((), Error)

        | String ->
            match op.Operator with
            | EqOp
            | NeqOp
            | LtOp
            | LeOp
            | GtOp
            | GeOp ->
                match bareType rightType with
                | String
                | Error ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"

                ((), Int)
            |_ ->
                ErrorMsg.Error op.Position "Unsupported operation on string"
                ((), Error)
    
        | Int ->
            match bareType rightType with
            | Int
            | Error ->
                ()
            | _ -> 
                ErrorMsg.Error op.Position "Type mismatch"

            ((), Int)
        | Unit ->
            ErrorMsg.Error op.Position "Type mismatch"
            ((), Int)
        | Name _ -> ErrorMsg.Impossible "Forbidden type in operator"

    let translateRecExp (exp:RecordExpType) =        
        match typeEnv.TryFind exp.Type with
        | None -> 
            ErrorMsg.Error exp.Position (sprintf "Unknown type %A" exp.Type)
            ((), Error)
        | Some type' ->
            match type' with
            | Record (fieldList, _) -> 
                if fieldList.Length = exp.Fields.Length then
                    List.iter2
                        (fun (expectedName, expectedType) (name, value, pos) ->
                            if expectedName = name then
                                let (trexp, type') = translateExp value env breakLabel
                                if typesMismatch type' expectedType then
                                    ErrorMsg.Error pos "Type mismatch"                                
                            else
                                ErrorMsg.Error pos (sprintf "Unexpected field %A. Expected %A." name expectedName))
                        fieldList
                        exp.Fields              
                else
                   ErrorMsg.Error exp.Position (sprintf "Length mismatch")
                ((), type')
            | _ -> 
                ErrorMsg.Error exp.Position (sprintf "%A is not a record type" exp.Type)
                ((), Error)

    let translateArrayExp (exp:ArrayExpType) =        
        match typeEnv.TryFind exp.Type with
        | None -> 
            ErrorMsg.Error exp.Position (sprintf "Unknown type %A" exp.Type)
            ((), Error)
        | Some type' ->
            match type' with
            | Array (elementType, _) -> 
                let (sizeExp, sizeType) = translateExp exp.Size env breakLabel
                let (initExp, initType) = translateExp exp.Init env breakLabel
                if typesMismatch sizeType Int then
                   ErrorMsg.Error exp.Position "Size must be an integer"
                else if typesMismatch initType elementType then
                   ErrorMsg.Error exp.Position "Element type does not match initial value" 
            
                ((), type')
            | _ -> 
                ErrorMsg.Error exp.Position (sprintf "%A is not an array type" exp.Type)
                ((), Error)

    let translateForExp (exp:ForExpType) =        
        let (lowExp, lowType) = translateExp exp.Low env breakLabel
        let (highExp, highType) = translateExp exp.High env breakLabel

        if typesMismatch lowType Int || typesMismatch highType Int then
            ErrorMsg.Error exp.Position "Bounds of for expression must be integers"

        let forVarEntry = VarEntry { Type = Int; CanAssign = false; }
    
        let augmentedEnv = (typeEnv, varEnv.Add exp.Var forVarEntry)    
        let (bodyExp, bodyType) = translateExp exp.Body augmentedEnv (Some true)
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body of for expression must not yield a value"

        ((), Unit)

    let translateWhileExp (exp:WhileExpType) =
        let (testExp, testType) = translateExp exp.Test env breakLabel
        let (bodyExp, bodyType) = translateExp exp.Body env (Some true)

        if typesMismatch testType Int then
            ErrorMsg.Error exp.Position "Test must have type Int"
        
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body must have type Unit"

        ((), Unit)

    let translateAssignExp (exp:AssignExpType) =
        //TODO: translateVar should return the trexp 
        let _, varType, canAssign = translateVar exp.Var env
        let (rhsExp, rhsType) = translateExp exp.Exp env breakLabel

        if not canAssign then
            ErrorMsg.Error exp.Position "Cannot assign to variable"

        if typesMismatch varType rhsType then
            ErrorMsg.Error exp.Position "Type mismatch"

        ((), Unit)

    let translateIfExp (exp:IfExpType) =
        let (typeEnv, varEnv) = env
        let (testExp, testType) = translateExp exp.Test env breakLabel
        let (thenExp, thenType) = translateExp exp.Then' env breakLabel
    
        if typesMismatch testType Int then  ErrorMsg.Error exp.Position "Test must evaluate to an integer" 
        match exp.Else' with
        | Some else' -> 
            let (elseExp, elseType) = translateExp else' env breakLabel
            if typesMismatch thenType elseType then 
                ErrorMsg.Error exp.Position (sprintf "Then and else types must match")
                ((), Error)
            else
                ((), thenType)
        | None ->
            if typesMismatch thenType Unit then 
                ErrorMsg.Error exp.Position (sprintf "Then clause without else must be unit")
            ((), Unit)

    match exp with
    | VarExp var -> 
        let (_, type', _) = translateVar var env
        ((), type')
    | NilExp ->
        ((), Type.Nil)
    | IntExp _ ->
        ((), Type.Int)
    | StringExp _ ->
        ((), Type.String)
    | CallExp call ->
        translateCall call env
    | OpExp op ->
        translateOperator op env
    | NegExp (exp, pos) ->
        let (trexp, type') = translateExp exp env breakLabel

        match type' with
        | Int
        | Error -> ()
        | _ -> ErrorMsg.Error pos "Type mismatch"        
        ((), Int)
    | RecordExp recExp ->
        translateRecExp recExp
    | ArrayExp arrExp ->
        translateArrayExp arrExp
    | SeqExp lst ->                
        List.fold (fun acc (exp, pos) -> translateExp exp env breakLabel) ((), Unit) lst
    | AssignExp exp ->
        translateAssignExp exp
    | IfExp exp ->
        translateIfExp exp
    | WhileExp exp ->
        translateWhileExp exp
    | ForExp exp ->
        translateForExp exp    
    | BreakExp pos ->
        match breakLabel with
        | Some value -> ()
        | None -> ErrorMsg.Error pos "Break must be enclosed in a loop"
        ((), Unit)
    | LetExp exp ->
        let updatedEnv = translateDecs exp.Decs env
        translateExp exp.Body updatedEnv breakLabel

let translateProg program =
    let (translated, expType) = translateExp program Environment.BaseEnv None

    // Programs have to return type Int
    if typesMismatch expType Int then
        let pos = new Position ("", 0L ,0L, 0L)
        ErrorMsg.Error pos "Program must have type Int"

    translated

let transProg program =
    ErrorMsg.reset

    translateProg program

        