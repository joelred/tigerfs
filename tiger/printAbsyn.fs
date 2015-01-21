module PrintAbsyn

open SymbolNS
open Absyn

let print (exp:Exp) =
    let rec indent i = 
        match i with
        | 0 -> ()
        | n -> printf "  "; indent (n-1)

    let operatorName op =
        match op with
        | PlusOp -> "PlusOp"
        | MinusOp -> "MinusOp"
        | MulOp -> "TimesOp"
        | DivOp -> "DivideOp"
        | EqOp -> "EqOp"
        | NeqOp -> "NeqOp"
        | LtOp -> "LtOp"
        | LeOp -> "LeOp"
        | GtOp -> "GtOp"
        | GeOp -> "GeOp"

    let rec doList depth f list' =
        match list' with
        | [a] -> 
            printfn ""
            f a (depth+1)
        | a::rest ->
            printfn ""
            f a (depth+1)
            printf ","
            doList depth f rest
        | [] ->
            ()

    let rec printVar v depth =
        match v with
        | SimpleVar (symbol, pos) ->
            indent depth; printf "SimpleVar(%s)" symbol.Name
        | FieldVar (var, symbol, pos) ->
            indent depth; printfn "FieldVar("
            printVar var (depth+1)
            indent (depth+1); printf "%s)" symbol.Name
        | SubscriptVar (var, exp, pos) ->
            indent depth; printfn "SubscriptVar("
            printVar var (depth+1); printfn ","
            printExp exp (depth+1); printf ")"
    and printExp e depth =
        match e with
        | VarExp var ->
            indent depth; printfn "VarExp("
            printVar var (depth+1)
            printf ") "
        | NilExp ->
            indent depth; printf "NilExp"
        | IntExp value ->
            indent depth; printf "IntExp(%d)" value
        | StringExp (value,pos) ->
            indent depth; printf "StringExp(\"%s\")" value
        | CallExp c ->
            indent depth; printf "CallExp(%s, [" c.Func.Name; doList depth printExp c.Args; printf "])"
        | NegExp (exp,_) ->
            indent depth; printfn "NegExp(";
            printExp exp depth; printf ")"
        | OpExp exp ->
            indent depth; printfn "OpExp(%s," (operatorName exp.Operator)
            printExp exp.Left (depth+1); printfn ","
            printExp exp.Right (depth+1); printf ")"
        | RecordExp exp ->
            let f (name : Symbol, exp, pos) depth =
                indent depth; printfn "%s" name.Name; 
                printExp exp (depth+1); printf ")"
            indent depth; printfn "RecordExp(%s,[" exp.Type.Name
            doList depth f exp.Fields; printf "])"
        | SeqExp list' ->
            indent depth; printf "SeqExp["; doList depth printExp (List.map fst list'); printf "]"
        | AssignExp exp ->
            indent depth; printfn "AssignExp("
            printVar exp.Var (depth+1); printfn ","
            printExp exp.Exp (depth+1); printf ")"
        | IfExp exp ->
            indent depth; printfn "IfExp("; printExp exp.Test (depth+1); printfn ","
            printExp exp.Then' (depth+1)
            match exp.Else' with
            | None -> ()
            | Some e -> 
                printfn ","
                printExp e (depth+1)
        | WhileExp exp ->
            indent depth; printfn "WhileExp("
            printExp exp.Test (depth+1); printfn ","
            printExp exp.Body (depth+1); printf ")"
        | ForExp exp ->
            indent depth; printfn "ForExp("
            printfn "%s, %b," exp.Var.Name exp.Escape
            printExp exp.Low (depth+1); printfn ","
            printExp exp.High (depth+1); printfn ","
            printExp exp.Body (depth+1); printf ")"
        | BreakExp p ->
            indent depth; printf "BreakExp"
        | LetExp exp ->
            indent depth; printf "LetExp(["
            doList depth printDec exp.Decs; printfn "],"
            printExp exp.Body (depth+1); printf ")"
        | ArrayExp exp ->
            indent depth; printfn "ArrayExp(%s," exp.Type.Name
            printExp exp.Size (depth+1); printfn ","
            printExp exp.Init (depth+1); printf ")"
   
    and printDec dec depth =
        match dec with
        | FunctionDec list' ->
            let printField field depth =
                indent depth; printf "(%s, %b, %s)" field.Name.Name field.Escape field.Type.Name
            let printFunctionDec (functionDec:FunctionDecType) depth =
                indent depth; printf "(%s,[" functionDec.Name.Name
                doList depth printField functionDec.Params; printfn "],"
                match functionDec.Result with 
                | None -> printfn "NONE"
                | Some (s,_) -> printfn "SOME(%s)" s.Name
                printExp functionDec.Body (depth+1)
            indent depth; printf "FunctionDec["; doList depth printFunctionDec list'; printf "]"
        | VarDec dec ->
            indent depth; printf "VarDec(%s, %b," dec.Name.Name dec.Escape
            match dec.Type with
            | None -> printfn "NONE,"
            | Some (s,_) -> printfn "SOME(%s)," s.Name
            printExp dec.Init (depth+1); printf ")"
        | TypeDec decList ->
            let printDec (dec : TypeDecType) depth =
                indent depth; printfn "(%s," dec.Name.Name
                printType dec.Type (depth+1); printf ")"
            indent depth; printf "TypeDec["; doList depth printDec decList; printf "]"
    and printType type' depth =
        match type' with
        | NameType (s,_) -> 
            indent depth; printf "NameType(%s)" s.Name
        | RecordType list' ->
            let printField field depth =
                indent depth; printf "(%s, %b, %s)" field.Name.Name field.Escape field.Type.Name
            indent depth; printf "RecordType["; doList depth printField list'; printf "]"
        | ArrayType (s,_) ->
            indent depth; printf "ArrayType(%s)" s.Name

    printExp exp 0; printfn ""
        
