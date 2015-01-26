module public Tiger.TigerExceptions

exception FatalException of string
exception ParseError of string
exception SemanticError of string