module internal Tiger.Environment

open Types
open SymbolNS

type VarEntryType = {
    Type : Type;
    CanAssign : bool; }

type FunEntryType = {
    Formals : Type list;
    Result : Type; }

type EnvBinding =
| VarEntry of VarEntryType
| FunEntry of FunEntryType

type TypeEnv = Types.Type SymbolNS.Table
type VarEnv = EnvBinding SymbolNS.Table

type Env = TypeEnv * VarEnv

let basicTypes = Map.ofList 
                  [ (Symbol.Symbol "int", Type.Int);
                    (Symbol.Symbol "string", Type.String); ]
 
let varMap = Map.ofList
                  [ (Symbol.Symbol "print", 
                        FunEntry { Formals = [String];
                          Result = Unit} );
                    (Symbol.Symbol "flush", 
                        FunEntry { Formals = [];
                          Result = Unit} );
                    (Symbol.Symbol "getchar", 
                        FunEntry { Formals = [];
                          Result = String} );
                    (Symbol.Symbol "ord", 
                       FunEntry  { Formals = [String];
                          Result = Int} );
                    (Symbol.Symbol "chr", 
                       FunEntry  { Formals = [Int];
                          Result = String} );
                    (Symbol.Symbol "size", 
                       FunEntry { Formals = [String];
                          Result = Int} );
                    (Symbol.Symbol "substring", 
                       FunEntry { Formals = [String; Int; Int];
                          Result = String} );
                    (Symbol.Symbol "concat", 
                       FunEntry { Formals = [String; String];
                          Result = Int} );
                    (Symbol.Symbol "not", 
                       FunEntry { Formals = [Int];
                          Result = Int} );
                    (Symbol.Symbol "exit", 
                       FunEntry { Formals = [Int];
                          Result = Unit} );
                        ]

let BaseEnv = (TypeEnv basicTypes, VarEnv varMap)