module internal Tiger.Tree

type Label = Temps.Label
type size = int

type Statement = 
    | SEQ of Statement * Statement
    | LABEL of Label
    | JUMP of Expression * Label list
    | CJUMP of Relop * Expression * Expression * Label * Label
    | MOVE of Expression * Expression
    | EXP of Expression

and Expression = 
    | BINOP of Binop * Expression * Expression
    | MEM of Expression
    | TEMP of Temps.Temp
    | ESEQ of Statement * Expression
    | NAME of Label
    | CONST of int
    | CALL of Expression * Expression list

and Binop = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
 
and Relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE