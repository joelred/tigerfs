module internal Tiger.Absyn

open Symbols

/// Position within the file
type internal Position = FParsec.Position

/// Abstract variable usage
type internal Var =
     | SimpleVar of Symbol * Position
     | FieldVar of Var * Symbol * Position
     | SubscriptVar of Var * Exp * Position
/// Abstract Expression
and internal Exp =
    | VarExp of Var
    | NilExp
    | IntExp of int64
    | StringExp of string * Position
    | CallExp of CallExpType
    | OpExp of OpExpType
    | NegExp of Exp * Position
    | RecordExp of RecordExpType
    | ArrayExp of ArrayExpType
    | SeqExp of (Exp * Position) list
    | AssignExp of AssignExpType
    | IfExp of IfExpType
    | WhileExp of WhileExpType
    | ForExp of ForExpType
    | BreakExp of Position
    | LetExp of LetExpType

and internal CallExpType = {
    Func: Symbol;
    Args: Exp list;
    Position: Position
    }
and internal OpExpType = {
    Left: Exp;
    Operator: Operator;
    Right: Exp; 
    Position: Position
    }
and internal RecordExpType = {
    Fields: (Symbol * Exp * Position) list;
    Type: Symbol;
    Position: Position
    }    
and internal ArrayExpType = {
    Type: Symbol;
    Size: Exp;
    Init: Exp;
    Position: Position
    }
and internal AssignExpType = {
    Var: Var;
    Exp: Exp;
    Position: Position
    }
and internal IfExpType = {
    Test: Exp;
    Then': Exp;
    Else': Exp option;
    Position: Position
    }
and internal WhileExpType = {
    Test: Exp;
    Body: Exp;
    Position: Position
    }
and internal ForExpType =  {
    Var: Symbol;
    mutable Escape: bool ;
    Low: Exp;
    High: Exp;
    Body: Exp;
    Position: Position
    }
and internal LetExpType = {
    Decs: Dec list;
    Body: Exp;
    Position: Position
    }
/// Abstract Declaration
and internal Dec = 
    | FunctionDec of FunctionDecType list
    | VarDec of VarDecType        
    | TypeDec of TypeDecType list
    
and internal FunctionDecType = {
    Name: Symbol;
    Params: Field list;
    Result: (Symbol * Position) option;
    Body: Exp;
    Position: Position
    }
     
and internal VarDecType= {
    Name: Symbol;
    mutable Escape: bool;
    Type: (Symbol * Position) option;
    Init: Exp;
    Position: Position
    }
and internal TypeDecType = {
    Name: Symbol;
    Type: Type;
    Position: Position
    }  
/// Abstract type definition
and internal Type = 
    | NameType of Symbol * Position
    | RecordType of Field list
    | ArrayType of Symbol * Position
and internal Field = {
    Name: Symbol;
    mutable Escape: bool;
    Type: Symbol;
    Position: Position
    }
and internal Operator = 
    | MulOp | DivOp
    | PlusOp | MinusOp 
    | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

