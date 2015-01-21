module TestParserAppel

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
type testParserAppel () =
    [<Test>]
    member self.testArrayLet () =  
        let str = @"//test1.tig
                    /* an array type and an array variable */
                     let
 	                    type  arrtype = array of int
	                    var arr1:arrtype := arrtype [10] of 0
                     in
	                    arr1
                     end"
    
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
       

    [<Test>]
    member self.testArrayLet2 () =
        let str = @"//test2.tig
                    /* arr1 is valid since expression 0 is int = myint */
                    let
	                    type myint = int
	                    type  arrtype = array of myint

	                    var arr1:arrtype := arrtype [10] of 0
                    in
	                    arr1
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
      
    [<Test>]
    member self.testRecordLet () =
        let str = @"//test3.tig
                    /* a record type and a record variable */
                    let
	                    type  rectype = {name:string, age:int}
	                    var rec1:rectype := rectype {name=""Nobody"", age=1000}
                    in
	                    rec1.name := ""Somebody"";
	                    rec1
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
        
    [<Test>]
    member self.testRecFuncLet () =
        let str = @"// test4.tig
                    /* define a recursive function */
                    let

                    /* calculate n! */
                    function nfactor(n: int): int =
		                    if  n = 0 
			                    then 1
			                    else n * nfactor(n-1)

                    in
	                    nfactor(10)
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
    
    [<Test>]
    member self.testRecTypeLet () =
        let str = @"// test5.tig
                    /* define valid recursive types */
                    let
                    type intlist = {hd: int, tl: intlist} 

                    // define a tree */
                    type tree ={key: int, children: treelist}
                    type treelist = {hd: tree, tl: treelist}

                    var lis:intlist := intlist { hd=0, tl= nil } 

                    in
	                    lis
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
    
    [<Test>]
    member self.testRecProcLet () =
        let str = @"// test6.tig
                    /* define valid mutually recursive procedures */
                    let

                    function do_nothing1(a: int, b: string)=
		                    do_nothing2(a+1)

                    function do_nothing2(d: int) =
		                    do_nothing1(d, ""str"")

                    in
	                    do_nothing1(0, ""str2"")
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
 
    [<Test>]
    member self.testMutuallyRecursiveFunctions () =
        let str = @"// test7.tig
                    /* define valid mutually recursive functions */
                    let

                    function do_nothing1(a: int, b: string):int=
		                    (do_nothing2(a+1);0)

                    function do_nothing2(d: int) =
		                    (do_nothing1(d, ""str"");"" "")

                    in
	                    do_nothing1(0, ""str2"")
                    end"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.testCorrectIf () =
        let str = @"// test8.tig
                    /* correct if */
                    if (10 > 20) then 30 else 40"

        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
    
    [<Test>]
    member self.testInvalidIf () =
        let str = @"// test9.tig
                    /* error : types of then - else differ */
                    if (5>4) then 13 else  "" "" "
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.testInvalidWhile () =
        let str = @"// test10.tig
                    /* error : body of while not unit */
                    while(10 > 5) do 5+6"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.testInvalidFor () =
        let str = @"// test11.tig
                   /* error hi expr is not int, and index variable erroneously assigned to.  */
                    for i:=10 to "" "" do 
	                    i := i - 1"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.testValidFor () =
        let str = @"// test12.tig
                   /* valid for and let */  
                    let
	                    var a := 0
                    in 
	                    for i :=0 to 100 do (a:=a+1;())
                    end"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.compareIncompatibleIntString () =
        let str = @"// test13.tig
                   /* error: comparison of incompatible types 
                      Parses but not valid code */
                    3 > ""df"""
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
    [<Test>]
    member self.compareIncompatibleRecArray () =
        let str = @"// test14.tig
                   /* error : compare rec with array */
                    let

	                    type arrtype = array of int
	                    type rectype = {name:string, id: int}

	                    var rec := rectype {name=""aname"", id=0}
	                    var arr := arrtype [3] of 0

                    in
	                    if rec <> arr then 3 else 4
                    end"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
     [<Test>]
     member self.IfThenNonUnitReturn () =
        let str = @"// test15.tig
                   /* error : if-then returns non unit */
                    if 20 then 3"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
   
     [<Test>]
     member self.InvalidMutuallyRecursiveTypes () =
        let str = @"// test16.tig
                   /* error: mutually recursive types thet do not pass through record or array */
                    let 

                    type a=c
                    type b=a
                    type c=d
                    type d=a

                    in
                     """"
                    end"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.InvalidRecursiveRecords () =
        let str = @"// test17.tig
                   /* error: definition of recursive types is interrupted */
                    let
                    /* define a tree */
                    type tree ={key: int, children: treelist}
                    var d:int :=0
                    type treelist = {hd: tree, tl: treelist}

                    in
	                    d
                    end"
       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.InvalidRecursiveFunctions () =
        let str = @"// test18.tig
                   /* error : definition of recursive functions is interrupted */
                    let

                    function do_nothing1(a: int, b: string):int=
		                    (do_nothing2(a+1);0)

                    var d:=0

                    function do_nothing2(d: int):string =
		                    (do_nothing1(d, ""str"");"" "")

                    in
	                    do_nothing1(0, ""str2"")
                    end"

       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.InvalidVariableScope () =
        let str = @"// test19.tig
                   /* error : second function uses variables local to the first one, undeclared variable */
                    let

                    function do_nothing1(a: int, b: string):int=
		                    (do_nothing2(a+1);0)

                    function do_nothing2(d: int):string =
		                    (do_nothing1(a, ""str"");"" "")

                    in
	                    do_nothing1(0, ""str2"")
                    end"

       
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.UndeclaredVar () =
        let str = @"// test20.tig
                   /* error: undeclared variable i */

                    while 10 > 5 do (i+1;())"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.ProcReturnsValue () =
        let str = @"// test21.tig
                    /* error : procedure returns value  and procedure is used in arexpr */
                    let

                    /* calculate n! */
                    function nfactor(n: int) =
		                    if  n = 0 
			                    then 1
			                    else n * nfactor(n-1)

                    in
	                    nfactor(10)
                    end "
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)


    [<Test>]
    member self.InvalidField () =
        let str = @"// test22.tig
                    /* error : field not in record type */

                    let 
	                    type rectype = {name:string , id:int}
	                    var rec1 := rectype {name=""Name"", id=0}
                    in
	                    rec1.nam := ""asd""
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)


    [<Test>]
    member self.FieldTypeMismatch () =
        let str = @"// test23.tig
                    /* error : type mismatch */
                    let 
	                    type rectype = {name:string , id:int}
	                    var rec1 := rectype {name=""aname"", id=0}
                    in
	                    rec1.name := 3;
	                    rec1.id := """" 
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.VariableNotArray () =
        let str = @"// test24.tig                    
                    /* error : variable not array */
                    let 
	                    var d:=0
                    in
	                    d[3]
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.VariableNotRecord () =
        let str = @"// test25.tig                    
                    /* error : variable not array */
                    let 
	                    var d:=0
                    in
	                    d.f
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.AddMismatch () =
        let str = @"// test26.tig                    
                    /* error : integer required */

                    3 + ""var"""
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.LocalsHideGlobals () =
        let str = @"// test27.tig                    
                   /* locals hide globals */
                    let
	                    var a:=0

	                    function g(a:int):int = a 
                    in
                     g(2)
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)