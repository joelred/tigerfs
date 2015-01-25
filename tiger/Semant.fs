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

let validateType type' (tenv : TypeEnv) =
    match type' with 
    | NameType (sym, pos) ->
        let type' = tenv.TryFind sym
        match type' with
        | Some type' ->
            type'
        | None ->
            ErrorMsg.Error pos "Declared type must match initializer"
            Error
    | RecordType fieldList ->
        let unique = Types.nextUnique
        let typeList =
            List.map 
                (fun (field : Field) -> 
                    let fieldType = tenv.TryFind field.Type
                    match fieldType with 
                    | Some fieldType ->
                        (field.Name, fieldType)
                    | None ->
                        ErrorMsg.Error field.Position "Unknown type"
                        (field.Name,Error))
                fieldList
        Record (typeList, unique)
    | ArrayType (baseTypeName, pos) ->
        let uniquifier = Types.nextUnique
        let baseType = tenv.TryFind baseTypeName
        match baseType with
        | Some type' ->
            Array (type', uniquifier)
        | None ->
            ErrorMsg.Error pos "Unknown type"
            Array(Error, uniquifier)        
                           
let rec validateDec dec (env : Env) : Env =
    let (tenv, venv) = env

    let rec validateFunctionDecs decs =
        //TODO insert functions
        env

    let validateVarDec dec =
        let (initExp, initType) = validateExp dec.Init env
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

    let rec validateTypeDecs decs = 
        // TODO: Insert type declarations
        env

    match dec with 
    | FunctionDec decs ->
        validateFunctionDecs decs       
    | VarDec var ->
        validateVarDec var
    | TypeDec decs ->
        validateTypeDecs decs

and validateDecs decs env =
    List.fold 
        (fun env dec -> validateDec dec env)
        env
        decs

and validateVar var (env : Env) =
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
        let (_, type', canAssign) = validateVar var env
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
        let (_, type', canAssign) = validateVar var env
        match bareType type' with
        | Array (type', _) ->
            ((), type', true)
        | Error -> 
            ((), Error, true)
        | _ -> 
            ErrorMsg.Error pos "Attempt to subscript in non-array type"
            ((), Error, true)

and validateExp exp (env : Env) : ExpTy =
    let (typeEnv, varEnv) = env
    
    let validateCall call (env : Env) =
        let (typeEnv, varEnv) = env

        match varEnv.TryFind call.Func with
        | Some (FunEntry f) -> 
            if( f.Formals.Length = call.Args.Length ) then
                List.iter2 
                    (fun expectedType receivedExp ->
                        let (_, receivedType) = validateExp receivedExp env
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

    let validateOperator op (env : Env) =        
        let (leftExp, leftType) = validateExp op.Left env
        let (rightExp, rightType) = validateExp op.Right env
      
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

    let validateRecExp (exp:RecordExpType) =        
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
                                let (trexp, type') = validateExp value env
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

    let validateArrayExp (exp:ArrayExpType) =        
        match typeEnv.TryFind exp.Type with
        | None -> 
            ErrorMsg.Error exp.Position (sprintf "Unknown type %A" exp.Type)
            ((), Error)
        | Some type' ->
            match type' with
            | Array (elementType, _) -> 
                let (sizeExp, sizeType) = validateExp exp.Size env
                let (initExp, initType) = validateExp exp.Init env
                if typesMismatch sizeType Int then
                   ErrorMsg.Error exp.Position "Size must be an integer"
                else if typesMismatch initType elementType then
                   ErrorMsg.Error exp.Position "Element type does not match initial value" 
            
                ((), type')
            | _ -> 
                ErrorMsg.Error exp.Position (sprintf "%A is not an array type" exp.Type)
                ((), Error)

    let validateForExp (exp:ForExpType) =        
        let (lowExp, lowType) = validateExp exp.Low env
        let (highExp, highType) = validateExp exp.High env

        if typesMismatch lowType Int || typesMismatch highType Int then
            ErrorMsg.Error exp.Position "Bounds of for expression must be integers"

        let forVarEntry = VarEntry { Type = Int; CanAssign = false; }
    
        let augmentedEnv = (typeEnv, varEnv.Add exp.Var forVarEntry)    
        let (bodyExp, bodyType) = validateExp exp.Body augmentedEnv
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body of for expression must not yield a value"

        ((), Unit)

    let validateWhileExp (exp:WhileExpType) =
        let (testExp, testType) = validateExp exp.Test env
        let (bodyExp, bodyType) = validateExp exp.Body env

        if typesMismatch testType Int then
            ErrorMsg.Error exp.Position "Test must have type Int"
        
        if typesMismatch bodyType Unit then
            ErrorMsg.Error exp.Position "Body must have type Unit"

        ((), Unit)

    let validateAssignExp (exp:AssignExpType) =
        //TODO: validateVar should return the trexp 
        let _, varType, canAssign = validateVar exp.Var env
        let (rhsExp, rhsType) = validateExp exp.Exp env

        if not canAssign then
            ErrorMsg.Error exp.Position "Cannot assign to variable"

        if typesMismatch varType rhsType then
            ErrorMsg.Error exp.Position "Type mismatch"

        ((), Unit)

    let validateIfExp (exp:IfExpType) =
        let (typeEnv, varEnv) = env
        let (testExp, testType) = validateExp exp.Test env
        let (thenExp, thenType) = validateExp exp.Then' env
    
        if typesMismatch testType Int then  ErrorMsg.Error exp.Position "Test must evaluate to an integer" 
        match exp.Else' with
        | Some else' -> 
            let (elseExp, elseType) = validateExp else' env
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
        let (_, type', _) = validateVar var env
        ((), type')
    | NilExp ->
        ((), Type.Nil)
    | IntExp _ ->
        ((), Type.Int)
    | StringExp _ ->
        ((), Type.String)
    | CallExp call ->
        validateCall call env
    | OpExp op ->
        validateOperator op env
    | NegExp (exp, pos) ->
        let (trexp, type') = validateExp exp env 

        match type' with
        | Int
        | Error -> ()
        | _ -> ErrorMsg.Error pos "Type mismatch"        
        ((), Int)
    | RecordExp recExp ->
        validateRecExp recExp
    | ArrayExp arrExp ->
        validateArrayExp arrExp
    | SeqExp lst ->                
        List.fold (fun acc (exp, pos) -> validateExp exp env) ((), Unit) lst
    | AssignExp exp ->
        validateAssignExp exp
    | IfExp exp ->
        validateIfExp exp
    | WhileExp exp ->
        validateWhileExp exp
    | ForExp exp ->
        validateForExp exp    
    | BreakExp pos ->
        ((), Unit)
    | LetExp exp ->
        let updatedEnv = validateDecs exp.Decs env
        validateExp exp.Body updatedEnv

let validateProg program =
    let (translated, expType) = validateExp program Environment.BaseEnv 

    // Programs have to return type Int
    if typesMismatch expType Int then
        let pos = new Position ("", 0L ,0L, 0L)
        ErrorMsg.Error pos "Program must have type Int"

    translated

let transProg program =
    ErrorMsg.reset

    validateProg program

        