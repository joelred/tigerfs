module internal Tiger.Escape

open Absyn
open Symbols

type Source = 
    | For of ForExpType
    | Param of Field
    | Var of VarDecType

type EscapeType =
    { Depth : int;
      Source : Source; }

type EscapeEnv = (int * Source) SymbolTable

let updateSource escape source =
    match source with
    | For f -> f.Escape <- escape
    | Param f -> f.Escape <- escape
    | Var v -> v.Escape <- escape

let FindEscape prog =
    let rec traverseVar (env:EscapeEnv) depth var =
        match var with
        | SimpleVar (s, _) -> 
            match env.TryFind s with
            | Some (declDepth, source) -> 
                if depth <> declDepth then
                    updateSource true source
            | None -> (* undeclared var -- will be caught later *) ()
        | FieldVar (v,_,_)     -> traverseVar env depth v
        | SubscriptVar (v,e,_) -> traverseVar env depth v
                                  traverseExp env depth e

    and traverseExp env depth exp =
        match exp with
        | VarExp v -> traverseVar env depth v
        | NilExp -> ()
        | IntExp _ -> ()
        | StringExp _ -> ()
        | CallExp e -> 
           List.iter (traverseExp env depth) e.Args
        | OpExp e -> 
            traverseExp env depth e.Left
            traverseExp env depth e.Right
        | NegExp (e,_) -> traverseExp env depth e
        | RecordExp e ->
            List.iter
                ( fun (_,exp,_) -> traverseExp env depth exp )
                e.Fields
        | ArrayExp e ->
            traverseExp env depth e.Size
            traverseExp env depth e.Init
        | SeqExp el ->
            List.iter
                (fun (exp,_) -> traverseExp env depth exp)
                el
        | AssignExp e ->
            traverseVar env depth e.Var
            traverseExp env depth e.Exp
        | IfExp e ->
            traverseExp env depth e.Test
            traverseExp env depth e.Then'
            match e.Else' with
            | Some exp -> traverseExp env depth exp
            | None -> ()
        | WhileExp e ->
            traverseExp env depth e.Test
            traverseExp env depth e.Body
        | ForExp e ->
            traverseExp env depth e.Low
            traverseExp env depth e.High
            let forEnv = 
                e.Escape <- false
                env.Add e.Var (depth, For e)
            traverseExp forEnv depth e.Body
        | BreakExp _ -> ()
        | LetExp e ->
            let letEnv = traverseDecs env depth e.Decs
            traverseExp letEnv depth e.Body

    and traverseDecs (env:EscapeEnv) depth (decs : Dec list)=        
        
        let traverseFunctions env decs =
           
            let traverseFunction env dec =
                //Add the params to create a function environment
                //The params are in the next level
                let funcEnv = 
                    List.fold 
                        (fun (env:EscapeEnv) param ->
                            param.Escape <- false
                            env.Add param.Name (depth+1, Param param))
                        env
                        dec.Params

                traverseExp funcEnv (depth+1) dec.Body          
                
            List.iter (traverseFunction env) decs

        let traverseDec (env:EscapeEnv) dec  = 
            match dec with
            | FunctionDec decs -> traverseFunctions env decs; env 
            | VarDec var -> 
                var.Escape <- false
                env.Add var.Name (depth, Var var)
            | TypeDec dec -> env

        List.fold traverseDec env decs

    traverseExp (new EscapeEnv()) 1 prog
    prog