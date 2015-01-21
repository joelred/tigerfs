module Parser

open SymbolNS
open Absyn
open FParsec
open FParsec.CharParsers

type UserState = unit
type Parser<'t> = Parser<'t,UserState>

// Comments
let commentLine : Parser<_> = 
    skipString "//" >>. skipRestOfLine true

let commentBlock, commentBlockImp = createParserForwardedToRef()
do commentBlockImp := 
    let eat x = skipCharsTillString x false System.Int32.MaxValue
    
    between
        (pstring "/*")
        (pstring "*/")
        (attempt (eat "/*" >>. commentBlock >>. eat "*/")  
            <|> (eat "*/"))

let position (pos:Position) = (pos.Line, pos.Column)

let getPosition = FParsec.CharParsers.getPosition |>> fun pos -> (pos.Line, pos.Column)

// Some abbreviations to make grammar productions more reasonable
let ws = (skipMany (choice [spaces1; commentBlock; commentLine])) <?> "whitespace"

let ch c = skipChar c >>. ws
let str s = pstring s .>> ws

let reservedWords = 
    Set.ofList
        ["type"; "array"; "of"; "var"; "function"; "nil";
        "let"; "in"; "end"; "if"; "then"; "else"; "while";
        "do"; "for"; "to"; "break"]
// An ident is [A-Z|a-z][A-Z|a-z|0-9|_]+
let ident : Parser<Symbol, _> =      
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  
    let identifierString = 
        many1Satisfy2 isLetter isIdentifierChar 
            .>> ws |>> Symbol.Symbol
    
    let isKeyword (str:Symbol) = 
        Set.contains (str.Name) reservedWords

    let expectedIdentifier = expected "identifier"
    fun stream ->
        let state = stream.State
        let reply = identifierString stream
        if reply.Status <> Ok || not (isKeyword reply.Result)
        then reply
        else 
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

// Primitives
let number = pint64 .>> ws 
 
let stringLit =
    let escape = anyOf "\"\\/bfnrt"
                 |>> function
                     | 'b' -> "\b"
                     | 'f' -> "\u000C"
                     | 'n' -> "\n"
                     | 'r' -> "\r"
                     | 't' -> "\t"
                     | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet) .>> ws

let keyword s = attempt (pstring s .>> notFollowedBy letter .>> notFollowedBy digit) .>> ws

// forward declaration of expressions
let expr, exprImp = createParserForwardedToRef()
let factor', factor'Imp = createParserForwardedToRef()


// Type declarations
let typeField = 
    pipe4 ident (str ":") ident getPosition 
        (fun name colon type' pos -> 
            {Name = name; 
                Escape = true;
                Type = type'; 
                Position = pos
                }) .>> ws

let typeFields = sepBy typeField (str ",")

let typeDec : Parser<TypeDecType,_> =

    let nameType = 
        pipe2 ident getPosition 
            (fun name pos -> NameType (name, pos))

    let recordType = 
        between
            (str "{")
            (str "}")
            typeFields |>> 
                (fun fl -> RecordType fl)

    let arrayType =
        let parser = keyword "array" >>. keyword "of" >>. ident
        pipe2 parser getPosition 
            (fun ident pos ->
                ArrayType (ident, pos))

    let ptype = 
        choice [nameType; recordType; arrayType]

    pipe3 (keyword "type" >>. ident ) getPosition (str "=" >>. ptype)
        (fun name pos type' -> 
            {Name = name; 
            Type = type';
            Position = pos}) <?> "Type Declaration"

// Variable declarations
let varDec =
    let resultType =
         pipe2 (str ":" >>. ident) getPosition
            (fun id pos -> (id, pos))
    
    pipe4 (keyword "var" >>. ident) getPosition (opt resultType) (str ":=" >>. expr) 
        (fun id pos type' expr -> 
            { Name = id;
              Escape = true;
              Type = type';
              Init = expr;
              Position = pos })

// Function Declarations
let functionDec =
    let resultType =
        pipe2 (str ":" >>. ident) getPosition
            (fun id pos -> (id, pos))

    let params' = 
        between
            (str "(")
            (str ")")
            typeFields

    pipe5 
        (keyword "function" >>. ident)
        getPosition 
        params'
        (opt resultType)
        (str "=" >>. expr)
        (fun name pos params' result body ->
            { Name = name;
              Params = params';
              Result = result;
              Body = body;
              Position = pos} )

// Declarations
let dec = 
    choiceL [
        many1 typeDec     |>> (fun decs -> TypeDec decs);
        varDec            |>> (fun dec -> VarDec dec);
        many1 functionDec |>> (fun decs -> FunctionDec decs);
    ] "declaration"




// LValues
// These are a little weird because they are naturally left recursive
type VarOffset =
    | FieldOffset of Symbol * Absyn.Position
    | SubscriptOffset of Exp * Absyn.Position

let lvalue =
    let rest, restImpl = createParserForwardedToRef()
    
    let restField =
         pipe2 (str "." >>. ident) getPosition
             (fun name pos -> FieldOffset (name, pos))
        
    let restSubscript = 
        pipe2 (between (str"[") (str "]") (expr .>> notFollowedBy (keyword "of"))) getPosition
            (fun exp pos -> SubscriptOffset (exp, pos))

    let prepend a optionList = 
        match optionList with
        | Some lst -> a::lst
        | None -> [a]
    
    do restImpl :=
        choice 
            [   
                pipe2 restField (opt rest) prepend                   
                pipe2 restSubscript (opt rest) prepend
            ]
    
    let rec buildVar var rest =
        match rest with
        | [] -> var
        | (FieldOffset (name,pos))::rest -> buildVar (FieldVar (var, name, pos)) rest
        | (SubscriptOffset (exp, pos))::rest -> buildVar (SubscriptVar(var, exp, pos)) rest

    pipe3 ident getPosition (opt rest)
        (fun name pos rest ->
            match rest with
            | None -> SimpleVar (name, pos)
            | Some l -> buildVar (SimpleVar (name, pos)) l
        )   

let strExpr = 
    pipe2 stringLit getPosition 
        (fun str pos -> 
            (str, pos))

let callExpr = 
    let paramList = 
        between 
            (str "(")
            (str ")")
            (sepBy expr (str ",") )

    pipe3 ident getPosition paramList
        (fun name pos params' ->
            { Func = name; Args = params'; Position = pos})
 
let expList = 
    (sepBy (expr .>>. getPosition) (str ";"))

let seqExpr =
    between 
        (str "(")
        (str ")")
        expList

let uminusExpr = 
    pipe2 (str "-" >>. getPosition) expr 
        (fun pos exp -> (exp, pos))

let ifExpr =
    pipe4 
        (keyword "if" >>. getPosition) 
        expr
        (keyword "then" >>. expr) 
        (opt (keyword "else" >>. expr))
        (fun pos test then' else' -> 
                {Test     = test;
                 Then'    = then';
                 Else'    = else';
                 Position = pos; })

let whileExpr =
    pipe3 
        (keyword "while" >>. getPosition)
        expr
        (keyword "do" >>. expr )
        (fun pos test body ->
            {Test = test;
             Body = body;
             Position = pos; })

let forExpr =
    pipe5 
        (keyword "for" >>. getPosition)
        ident
        (str ":=" >>. expr)
        (keyword "to" >>. expr)
        (keyword "do" >>. expr)
        (fun pos var low high body ->
            {Var = var;
             Escape = true;
             Low = low;
             High = high;
             Body = body;
             Position = pos})

let breakExpr =
    keyword "break" >>. getPosition

let letExpr = 
    pipe3
        (keyword "let" >>. getPosition)
        (many dec)
        (keyword "in" >>. expList )
        (fun pos decs expList ->
            let body = match expList with
                       | [(e, pos)] -> e
                       | _ -> SeqExp expList

            {Decs = decs;
             Body = body;
             Position = pos; })

let factor =
    choice [
        keyword "nil" |>> fun a -> NilExp;
        number |>> IntExp;
        strExpr |>> StringExp;
        seqExpr |>> SeqExp;
        attempt callExpr |>> CallExp;
        uminusExpr |>> NegExp;
        ifExpr |>> IfExp;
        whileExpr |>> WhileExp;
        forExpr |>> ForExp;
        breakExpr |>> BreakExp;
        letExpr |>> LetExp;
        lvalue |>> VarExp;
    ]

type MathRhs = Operator * Absyn.Position * Exp 
    
do factor'Imp :=
    let buildExp lhs (op, pos, rhs) =
        OpExp
            {Left = lhs;
             Operator = op;
             Right = rhs;
             Position = pos; }

    let binopRhs strOp op = 
        pipe3 (str strOp >>. getPosition) factor factor'
            (fun pos exp rhs ->
                match rhs with 
                | None -> Some (op, pos, exp)
                | Some e -> Some (op, pos, buildExp exp e))
    
    choice [
        binopRhs "*" MulOp;
        binopRhs "/" DivOp;
        preturn None ]


do exprImp := 
    factor

let prog = expr .>> ws .>> eof
