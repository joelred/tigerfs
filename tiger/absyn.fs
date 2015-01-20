module Absyn

open SymbolNS

/// Position within the file
type Position = int64*int64

/// Abstract variable usage
type Var =
     | SimpleVar of Symbol * Position
     | FieldVar of Var * Symbol * Position
     | SubscriptVar of Var * Exp * Position
/// Abstract Expression
and Exp =
    | VarExp of Var
    | NilExp
    | IntExp of int64
    | NegExp of Exp * Position
    | StringExp of string * Position
    | CallExp of CallExpType
    | OpExp of OpExpType
    | RecordExp of RecordExpType
    | SeqExp of (Exp * Position) list
    | AssignExp of AssignExpType
    | IfExp of IfExpType
    | WhileExp of WhileExpType
    | ForExp of ForExpType
    | BreakExp of Position
    | LetExp of LetExpType
    | ArrayExp of ArrayExpType

and CallExpType = {
    Func: Symbol;
    Args: Exp list;
    Position: Position
    }
and OpExpType = {
    Left: Exp;
    Operator: Operator;
    Right: Exp; 
    Position: Position
    }
and RecordExpType = {
    Fields: (Symbol * Exp * Position) list;
    Type: Symbol;
    Position: Position
    }    
and AssignExpType = {
    Var: Var;
    Exp: Exp;
    Position: Position
    }
and IfExpType = {
    Test: Exp;
    Then': Exp;
    Else': Exp option;
    Position: Position
    }
and WhileExpType = {
    Test: Exp;
    Body: Exp;
    Position: Position
    }
and ForExpType =  {
    Var: Symbol;
    mutable Escape: bool ;
    Low: Exp;
    High: Exp;
    Body: Exp;
    Position: Position
    }
and LetExpType = {
    Decs: Dec list;
    Body: Exp;
    Position: Position
    }
and ArrayExpType = {
    Type: Symbol;
    Size: Exp;
    Init: Exp;
    Position: Position
    }
/// Abstract Declaration
and Dec = 
    | FunctionDec of FunctionDecType list
    | VarDec of VarDecType        
    | TypeDec of TypeDecType list
    
and FunctionDecType = {
    Name: Symbol;
    Params: Field list;
    Result: (Symbol * Position) option;
    Body: Exp;
    Position: Position
    }
     
and VarDecType= {
    Name: Symbol;
    mutable Escape: bool;
    Type: (Symbol * Position) option;
    Init: Exp;
    Position: Position
    }
and TypeDecType = {
    Name: Symbol;
    Type: Type;
    Position: Position
    }  
/// Abstract type definition
and Type = 
    | NameType of Symbol * Position
    | RecordType of Field list
    | ArrayType of Symbol * Position
and Field = {
    Name: Symbol;
    mutable Escape: bool;
    Type: Symbol;
    Position: Position
    }
and Operator = 
    | MulOp | DivOp
    | PlusOp | MinusOp 
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

