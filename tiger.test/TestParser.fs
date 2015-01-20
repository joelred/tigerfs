module TestParser

open Absyn
open Parser
open FParsec

open NUnit.Framework
open FsUnit

let resultType pr = 
    match pr with    
    | Success (result,_,_) -> Some 1    
    | _ -> None


let result pr = 
    match pr with    
    | Success (result,_,_) -> Some result    
    | _ -> None

let runToEnd parser str = 
    run (parser .>> eof) str

[<TestFixture>]
type testParser () =
  
    [<Test>]
    member self.testCommentBlock () =
        let parseResult = runToEnd commentBlock "/* testing */"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd commentBlock "/* testing \n*/"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd commentBlock "/* asbdsg"
        resultType parseResult |> should equal None        

        let parseResult = runToEnd commentBlock "a"
        resultType parseResult |> should equal None        

        let parseResult = runToEnd commentBlock "/* /*abcd */*/"
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.testCommentLine () =
        let parseResult = runToEnd commentLine "// testing"
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.testIdent () = 
        let parseResult = runToEnd ident "a"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd ident ""
        resultType parseResult |> should equal (None)

        let parseResult = runToEnd ident "a123"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd ident "123"
        resultType parseResult |> should equal (None)

        let parseResult = runToEnd ident "if"
        resultType parseResult |> should equal (None)

    [<Test>]
    member self.testNumber () =
        let parseResult = runToEnd number "0x1234567812345678"
        resultType parseResult |> should equal (Some 1)
        
        let parseResult = runToEnd number "0x12345678123456781"
        resultType parseResult |> should equal None
        
    [<Test>]
    member self.testKeyword () =
        let parseResult = run (keyword "if") "if "
        resultType parseResult |> should equal (Some 1)

        let parseResult = run (keyword "if") "ifA"
        resultType parseResult |> should equal None

        let parseResult = run (keyword "if") "if1"
        resultType parseResult |> should equal None

        let parseResult = run (keyword "if") "if("
        resultType parseResult |> should equal  (Some 1)

    [<Test>]
    member self.testTypeField () =
        let parseResult = runToEnd typeField "a:b"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd typeField "a : b "
        resultType parseResult |> should equal (Some 1)

    member self.getType (result : TypeDecType option) =
        match result with 
        | Some f -> Some f.Type
        | None -> None

    [<Test>]
    member self.testNameType () =
        let parseResult = runToEnd typeDec "type a = a"
        resultType parseResult |> should equal (Some 1)
        match self.getType (result parseResult) with
        | Some (NameType (sym, _)) -> sym.Name |> should equal "a"
        | _ -> Assert.Fail("Unexpected type")
            

    [<Test>]
    member self.testRecordType () =
        let parseResult = runToEnd typeDec "type a = { } "
        resultType parseResult |> should equal (Some 1)
        match self.getType (result parseResult) with
        | Some (RecordType recordList) -> recordList |> should equal []
        | _ -> Assert.Fail("Unexpected type")

        let parseResult = runToEnd typeDec "type a = {a:b} "
        resultType parseResult |> should equal (Some 1)
        match self.getType (result parseResult) with
        | Some (RecordType [field]) -> 
            field.Name.Name |> should equal "a"
            field.Type.Name |> should equal "b"
        | _ -> Assert.Fail("Unexpected type")
            
        let parseResult = runToEnd typeDec "type a = { a : b } "
        resultType parseResult |> should equal (Some 1)
        match self.getType (result parseResult) with
        | Some (RecordType [field]) -> 
            field.Name.Name |> should equal "a"
            field.Type.Name |> should equal "b"
        | _ -> Assert.Fail("Unexpected type")
            
        let parseResult = runToEnd typeDec "type a = { a : b, c : d } "
        resultType parseResult |> should equal (Some 1)
        match  self.getType (result parseResult) with
        | Some (RecordType [field1; field2]) -> 
            field1.Name.Name |> should equal "a"
            field1.Type.Name |> should equal "b"
            field2.Name.Name |> should equal "c"
            field2.Type.Name |> should equal "d"
        | _ -> Assert.Fail("Unexpected type")
            
    [<Test>]
    member self.testArrayType () =
        let parseResult = run typeDec "type b = array of a"
        resultType parseResult |> should equal (Some 1)
        match  self.getType (result parseResult) with
        | Some (ArrayType (sym, _)) -> sym.Name |> should equal "a"
        | _ -> Assert.Fail("Unexpected type")

        let parseResult = runToEnd typeDec "type a = arrayof a"
        resultType parseResult |> should equal None

        let parseResult = runToEnd typeDec "type a = array ofa"
        resultType parseResult |> should equal None

    [<Test>]
    member self.testTypeDec () =
        let parseResult = runToEnd typeDec "type /*a*/  \n a  =\n   b "
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.testLvalue () =
        let parseResult = runToEnd lvalue "a.b[1]"
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.testOperator () =
        let parseResult = runToEnd opExpr ""
        resultType parseResult |> should equal None


        let parseResult = runToEnd opExpr "a + 1*c"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd opExpr "a = 1 = c"
        resultType parseResult |> should equal (None)

    [<Test>]
    member self.testExpr () =
        let parseResult = runToEnd expr "nil"
        resultType parseResult |> should equal (Some 1)

        let parseResult = runToEnd expr "(1)"
        resultType parseResult |> should equal (Some 1)

        
    [<Test>]
    member self.testEmpty () =    
        let parseResult = runToEnd expr "~"
        resultType parseResult |> should equal (Some 1)
              
        //let parseResult = runToEnd expr ""
        //resultType parseResult |> should equal (Some 1)
        