module internal Tiger.Semant

open Absyn
open Environment
open ErrorMsgs
open Translate
open Types

type ExpTy = Translate.Exp * Types.Type

let getTypeByName name pos (tenv: TypeEnv) = 
    let type' = tenv.TryFind name
    match type' with
    | Some t ->
        t
    | None -> 
        ErrorMsg.Error pos (sprintf "Unknown type: %A" name)
        Top

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
    | NameType (sym, pos) -> getTypeByName sym pos tenv
    | RecordType fieldList ->
        let unique = Types.nextUnique ()
        let typeList =
            //TODO: Make sure field names are unique

            List.map 
                (fun (field : Field) -> 
                    let fieldType = getTypeByName field.Type field.Position tenv
                    (field.Name, fieldType))                    
                fieldList
        Record (typeList, unique)
    | ArrayType (baseTypeName, pos) ->
        let uniquifier = Types.nextUnique ()
        let baseType = getTypeByName baseTypeName pos tenv
        Array(baseType, uniquifier)  
                           
let rec translateDec (env : Env) level dec: Env =
    let (tenv, venv) = env

    let rec translateFunctionDecs level (decs : FunctionDecType list)  =
        //First make sure decs doesn't contain any duplicate names
        let namepos = List.map (fun (dec : FunctionDecType) -> (dec.Name, dec.Position)) decs
        if not <| hasDuplicateNames namepos "Duplicate function name %A found in group" then

            /// Get just the function signature without worrying about the body
            let getSignature venv (dec:FunctionDecType) = 
                let resultType =
                    match dec.Result with
                    | Some (typeName, pos) -> getTypeByName typeName pos tenv                
                    | None -> Unit
            
                let paramTypes = List.map 
                                    (fun (param:Field) -> getTypeByName param.Type param.Position tenv)
                                    dec.Params
             
                // Create new level for function
                let formalEscapes = 
                    List.map 
                        (fun (var : Field) -> var.Escape)
                        dec.Params

                let label = new Temps.Label (dec.Name.Name)

                {  Level = Translate.NewLevel level label formalEscapes;
                   Label = label; 
                   Formals = paramTypes; 
                   Result = resultType; }
       
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
                
                let funcLevel = funEntry.Level
                
                // Insert the parameters into the namespace
                let functionVenv =
                    List.fold2 
                        (fun (venv:VarEnv) (field: Field) type' -> 
                            venv.Add 
                                field.Name 
                                (VarEntry 
                                    {Access = AllocateLocal funcLevel field.Escape
                                     Type = type'; 
                                     CanAssign = true}) )
                        functionsVarEnv
                        func.Params
                        funEntry.Formals
            
                let (bodyExp, bodyType) = translateExp (tenv, functionVenv) None level func.Body

                if typesMismatch bodyType funEntry.Result then
                    ErrorMsg.Error func.Position "Body type doesn't match declared type"
            
            List.iter translateFunction decs
            (tenv,functionsVarEnv)
        else
            (tenv,venv)

    let translateVarDec level dec =
        let (initExp, initType) = translateExp env None level dec.Init
        match dec.Type with
        | Some (declaredTypeName,pos) ->
            let declaredType = tenv.TryFind declaredTypeName
            match declaredType with
            | Some type' ->
                if typesMismatch type' initType then
                    ErrorMsg.Error dec.Position "Declared type must match initializer"
                    (tenv, venv.Add dec.Name (VarEntry 
                                                { Access = AllocateLocal level dec.Escape;
                                                  Type = Top; 
                                                  CanAssign = true }))
                else
                    (tenv, venv.Add dec.Name (VarEntry
                                                 { Access = AllocateLocal level dec.Escape;
                                                   Type = type';
                                                   CanAssign = true }))                
            | None ->                
                ErrorMsg.Error dec.Position (sprintf "Declared type %A does not exist" declaredTypeName)
                (tenv, venv.Add dec.Name (VarEntry 
                                            { Access = AllocateLocal level dec.Escape;
                                              Type = Top; 
                                              CanAssign = true }))   
        | None -> 
            match initType with
            | Nil -> 
                ErrorMsg.Error dec.Position ("Cannot assign nil to unknown type")
                (tenv, venv.Add dec.Name (VarEntry
                                             { Access = AllocateLocal level dec.Escape;
                                               Type = Top; 
                                               CanAssign = true }))
            | _ ->
                (tenv, venv.Add dec.Name (VarEntry 
                                             { Access = AllocateLocal level dec.Escape;
                                               Type = initType;
                                               CanAssign = true }))   

    let translateTypeDecs (decs:TypeDecType list) =
        let pos = match decs with
                  | a::rest -> a.Position
                  | [] -> Position ("", 0L, 0L, 0L) // Should never be used

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
                
            // Flatten the names and detect cycles
            let rec resolve type' (path : Symbols.Symbol list) : Type =
                match type' with
                | Name alias ->     
                    if List.exists (fun ty -> ty = alias.Name) path then
                        ErrorMsg.Error pos "Cycle in this group of type definitions"
                        Top      
                    else                                          
                        match alias.Type with   
                        | Some (Name alias') ->                                                                                                      
                            let resolvedType = resolve (Name alias') (alias.Name :: path)
                            alias.Type <- Some resolvedType
                            resolvedType
                        | Some knownType -> knownType
                        | None -> 
                            ErrorMsg.Impossible "Unknown type when flattening names"
                | _ -> type'
                        
            augmentedTypeEnv.ForEach (fun k ty -> ignore <| resolve ty [])
        
            (augmentedTypeEnv, venv)
        else
            env

    match dec with 
    | FunctionDec decs ->
        translateFunctionDecs level decs       
    | VarDec var ->
        translateVarDec level var
    | TypeDec decs ->
        translateTypeDecs decs

and translateDecs env level decs =
    List.fold 
        (fun env dec -> translateDec env level dec)
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
            ((), Top, false)          
        | None ->
            ErrorMsg.Error pos (sprintf "%A is not defined" sym)
            ((), Top, true)
    
    | FieldVar (var, fieldName, pos) ->
        let (_, type', canAssign) = translateVar var env
        match bareType type' with
        | Record (fieldList, _) ->
            match List.tryFind (fun (sym, type') -> fieldName = sym) fieldList with
            | Some (sym, type') -> ((), type', true)
            | None -> 
                ErrorMsg.Error pos (sprintf "%A is not a field in this record" fieldName)
                ((), Top, true)
        | Top -> ((), Top, true)
        | _ -> 
            ErrorMsg.Error pos "Attempt to assign field in non-record type"
            ((), Top, true)

    | SubscriptVar (var, subscript, pos) ->
        let (_, type', canAssign) = translateVar var env
        match bareType type' with
        | Array (type', _) ->
            ((), type', true)
        | Top -> 
            ((), Top, true)
        | _ -> 
            ErrorMsg.Error pos "Attempt to subscript in non-array type"
            ((), Top, true)

and translateExp (env : Env) (breakLabel : bool option) level exp : ExpTy =
    let (typeEnv, varEnv) = env
    
    let translateCall call (env : Env) =
        let (typeEnv, varEnv) = env

        match varEnv.TryFind call.Func with
        | Some (FunEntry f) -> 
            if( f.Formals.Length = call.Args.Length ) then
                List.iter2 
                    (fun expectedType receivedExp ->
                        let (_, receivedType) = translateExp env None level receivedExp
                        if typesMismatch receivedType expectedType then
                            ErrorMsg.Error call.Position (sprintf "Type mismatch in arguments to %A" call.Func) )
                    f.Formals
                    call.Args
            else 
                ErrorMsg.Error call.Position (sprintf "Expected %d args, received %d" f.Formals.Length call.Args.Length)                

            ((), f.Result)

        | _ -> 
            ErrorMsg.Error call.Position (sprintf "Function expected at %A" call.Func)
            ((), Top)

    let translateOperator op (env : Env) =        
        let (leftExp, leftType) = translateExp env breakLabel level op.Left 
        let (rightExp, rightType) = translateExp env breakLabel level op.Right 
      
        match bareType leftType with
        | Top -> 
            ((), Top)
        | Record (_,lu) ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Record(_,ru ) ->
                    if lu <> ru 
                    then ErrorMsg.Error op.Position "Records not of the same type"                
                | Nil 
                | Top ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"
                
                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on record"
                ((), Top)
        | Array (_,lu) ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Array(_,ru ) ->
                    if lu <> ru 
                    then ErrorMsg.Error op.Position "Arrays not of the same type"                                           
                | Top -> 
                    ()                
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"            
            
                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on array"
                ((), Top)
        | Nil ->
            match op.Operator with
            | EqOp
            | NeqOp ->
                match bareType rightType with
                | Record _                                              
                | Top ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"

                ((), Int)
            | _ ->
                ErrorMsg.Error op.Position "Unsupported operation on record"
                ((), Top)

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
                | Top ->
                    ()
                | _ -> 
                    ErrorMsg.Error op.Position "Type mismatch"

                ((), Int)
            |_ ->
                ErrorMsg.Error op.Position "Unsupported operation on string"
                ((), Top)
    
        | Int ->
            match bareType rightType with
            | Int
            | Top ->
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
            ((), Top)
        | Some type' ->
            match bareType type' with
            | Record (fieldList, _) -> 
                if fieldList.Length = exp.Fields.Length then
                    List.iter2
                        (fun (expectedName, expectedType) (name, value, pos) ->
                            if expectedName = name then
                                let (trexp, type') = translateExp env breakLabel level value 
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
                ((), Top)

    let translateArrayExp (exp:ArrayExpType) =        
        match typeEnv.TryFind exp.Type with
        | None -> 
            ErrorMsg.Error exp.Position (sprintf "Unknown type %A" exp.Type)
            ((), Top)
        | Some type' ->
            match bareType type' with
            | Array (elementType, _) -> 
                let (sizeExp, sizeType) = translateExp env breakLabel level exp.Size 
                let (initExp, initType) = translateExp env breakLabel level exp.Init 
                if typesMismatch sizeType Int then
                   ErrorMsg.Error exp.Position "Size must be an integer"
                else if typesMismatch initType elementType then
                   ErrorMsg.Error exp.Position "Element type does not match initial value" 
            
                ((), type')
            | _ -> 
                ErrorMsg.Error exp.Position (sprintf "%A is not an array type" exp.Type)
                ((), Top)

    let translateForExp (exp:ForExpType) =        
        let (lowExp, lowType) = translateExp env breakLabel level exp.Low 
        let (highExp, highType) = translateExp env breakLabel level exp.High 

        if typesMismatch lowType Int || typesMismatch highType Int then
            ErrorMsg.Error exp.Position "Bounds of for expression must be integers"
        
        let forVarEntry = VarEntry 
                            { Access = Translate.AllocateLocal level exp.Escape; 
                              Type = Int;
                              CanAssign = false; }
    
        let augmentedEnv = (typeEnv, varEnv.Add exp.Var forVarEntry)    
        let (bodyExp, bodyType) = translateExp augmentedEnv (Some true) level exp.Body 
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body of for expression must not yield a value"

        ((), Unit)

    let translateWhileExp (exp:WhileExpType) =
        let (testExp, testType) = translateExp env breakLabel level exp.Test 
        let (bodyExp, bodyType) = translateExp env (Some true) level exp.Body 

        if typesMismatch testType Int then
            ErrorMsg.Error exp.Position "Test must have type Int"
        
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body must have type Unit"

        ((), Unit)

    let translateAssignExp (exp:AssignExpType) =
        //TODO: translateVar should return the trexp 
        let _, varType, canAssign = translateVar exp.Var env
        let (rhsExp, rhsType) = translateExp env breakLabel level exp.Exp 

        if not canAssign then
            ErrorMsg.Error exp.Position "Cannot assign to variable"

        if typesMismatch varType rhsType then
            ErrorMsg.Error exp.Position "Type mismatch"

        ((), Unit)

    let translateIfExp (exp:IfExpType) =
        let (typeEnv, varEnv) = env
        let (testExp, testType) = translateExp env breakLabel level exp.Test 
        let (thenExp, thenType) = translateExp env breakLabel level exp.Then' 
    
        if typesMismatch testType Int then  ErrorMsg.Error exp.Position "Test must evaluate to an integer" 
        match exp.Else' with
        | Some else' -> 
            let (elseExp, elseType) = translateExp env breakLabel level else' 
            if typesMismatch thenType elseType then 
                ErrorMsg.Error exp.Position (sprintf "Then and else types must match")
                ((), Top)
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
        let (trexp, type') = translateExp env breakLabel level exp 

        match type' with
        | Int
        | Top -> ()
        | _ -> ErrorMsg.Error pos "Type mismatch"        
        ((), Int)
    | RecordExp recExp ->
        translateRecExp recExp
    | ArrayExp arrExp ->
        translateArrayExp arrExp
    | SeqExp lst ->                
        List.fold (fun acc (exp, pos) -> translateExp env breakLabel level exp) ((), Unit) lst
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
        let updatedEnv = translateDecs env level exp.Decs
        translateExp updatedEnv breakLabel level exp.Body

let translateProg program =
    let mainLevel = NewLevel Outermost (new Temps.Label ("t_main")) []

    let (translated, expType) = translateExp Environment.BaseEnv None mainLevel program
    
    if ErrorMsg.HasErrors then
        ErrorMsg.PrintCount
        raise (TigerExceptions.SemanticError "Semantic errors found")
    
    translated

let transProg program =
    ErrorMsg.reset

    translateProg program 

        