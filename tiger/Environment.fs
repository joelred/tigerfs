module internal Tiger.Environment

open Symbols
open Translate
open Types

type VarEntryType = {
    Access : Translate.Access
    Type : Type;
    CanAssign : bool; }

type FunEntryType = {
    Level : Translate.Level;
    Label : Temps.Label;
    Formals : Type list;
    Result : Type; }

type EnvBinding =
| VarEntry of VarEntryType
| FunEntry of FunEntryType

type TypeEnv = Types.Type Symbols.SymbolTable
type VarEnv = EnvBinding Symbols.SymbolTable

type Env = TypeEnv * VarEnv

let basicTypes = Map.ofList 
                  [ (Symbol.Symbol "int", Type.Int);
                    (Symbol.Symbol "string", Type.String); ]
 
let predefined name formals result =
    (Symbol.Symbol name,
            FunEntry { 
                Level = Outermost; 
                Label = new Temps.Label (name)
                Formals = formals;
                    Result = result} );

let varMap = Map.ofList
                  [ predefined "print" 
                               [String]
                               Unit;
                    predefined "flush"
                               []
                               Unit;
                    predefined "getchar"
                               []
                               String;
                    predefined "ord"
                               [String]
                               Int;
                    predefined "chr"
                               [Int]
                               String;
                    predefined "size"
                               [String]
                               Int;
                    predefined "substring"
                               [String; Int; Int]
                               String
                    predefined "concat"
                               [String; String]
                               Int
                    predefined "not"
                               [Int]
                               Int
                    predefined "exit"
                               [Int]
                               Unit   ]

let BaseEnv = (TypeEnv basicTypes, VarEnv varMap)