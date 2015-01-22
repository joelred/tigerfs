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

    [<Test>]
    member self.DifferentRecordTypes () =
        let str = @"// test28.tig                    
                   /* error : different record types */
                    let
	                    type rectype1 = {name:string , id:int}
	                    type rectype2 = {name:string , id:int}

	                    var rec1: rectype1 := rectype2 {name=""Name"", id=0}
                    in
	                    rec1
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.DifferentArrayTypes () =
        let str = @"// test29.tig                    
                   /* error : different array types */

                    let
	                    type arrtype1 = array of int
	                    type arrtype2 = array of int

	                    var arr1: arrtype1 := arrtype2 [10] of 0
                    in
	                    arr1
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.Synonyms () =
        let str = @"// test30.tig                    
                   /* synonyms are fine */
                    let 
		                    type a = array of int
		                    type b = a

		                    var arr1:a := b [10] of 0
                    in
		                    arr1[2]
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.VarDeclInitTypeCompatibility () =
        let str = @"// test31.tig                    
                   /* error : type constraint and init value differ */
                    let 
	                    var a:int := "" ""
                    in
	                    a
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.ExpArrayTypeCompatibility () =
        let str = @"// test32.tig                    
                   /* error : initializing exp and array type differ */

                    let
	                    type arrayty = array of int

	                    var a := arrayty [10] of "" ""
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.UnknownType () =
        let str = @"// test33.tig                    
                   /* error : unknown type */
                    let
	                    var a:= rectype {}
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.FunctionParamTypeMismatch () =
        let str = @"// test34.tig    
                    /* error : formals and actuals have different types */
                    let
	                    function g (a:int , b:string):int = a
                    in
	                    g(""one"", ""two"")
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.FunctionParamCountMismatch () =
        let str = @"// test35.tig    
                    /* error : formals are more then actuals */
                    let
	                    function g (a:int , b:string):int = a
                    in
	                    g(""one"")
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.FunctionParamCountMismatch2 () =
        let str = @"// test36.tig    
                    /* error : formals are more then actuals */
                    let
	                    function g (a:int , b:string):int = a
                    in
	                    g(3, ""one"", 5)
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.NameHiding () =
        let str = @"// test37.tig    
                    /* redeclaration of variable; this is legal, there are two different
                        variables with the same name.  The second one hides the first.  */
                    let
	                    var a := 0
	                    var a := "" ""
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.TypeHiding () =
        let str = @"// test38.tig    
                    /* This is illegal, since there are two types with the same name
                        in the same (consecutive) batch of mutually recursive types. 
                        See also test47  */
                    let
	                    type a = int
	                    type a = string
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.FunctionHiding () =
        let str = @"// test39.tig    
                    /* This is illegal, since there are two functions with the same name
                        in the same (consecutive) batch of mutually recursive functions.
                       See also test48 */
                    let
	                    function g(a:int):int = a
	                    function g(a:int):int = a
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.ProcReturnsValue2 () =
        let str = @"// test40.tig    
                    /* error : procedure returns value */
                    let
	                    function g(a:int) = a
                    in 
	                    g(2)
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.NestedLetLocalHidesGlobal () =
        let str = @"// test41.tig    
                    /* local types hide global */
                    let
	                    type a = int
                    in
	                    let
		                    type a = string
	                    in
		                    0
	                    end
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.CorrectDeclarations () =
        let str = @"// test42.tig    
                    /* correct declarations */
                    let 

                    type arrtype1 = array of int
                    type rectype1 = {name:string, address:string, id: int , age: int}
                    type arrtype2 = array of rectype1
                    type rectype2 = {name : string, dates: arrtype1}

                    type arrtype3 = array of string

                    var arr1 := arrtype1 [10] of 0
                    var arr2  := arrtype2 [5] of rectype1 {name=""aname"", address=""somewhere"", id=0, age=0}
                    var arr3:arrtype3 := arrtype3 [100] of """"

                    var rec1 := rectype1 {name=""Kapoios"", address=""Kapou"", id=02432, age=44}
                    var rec2 := rectype2 {name=""Allos"", dates= arrtype1 [3] of 1900}

                    in

                    arr1[0] := 1; 
                    arr1[9] := 3;
                    arr2[3].name := ""kati"";
                    arr2[1].age := 23;
                    arr3[34] := ""sfd"";

                    rec1.name := ""sdf"";
                    rec2.dates[0] := 2323;
                    rec2.dates[2] := 2323

                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.UnitIntMismatch () =
        let str = @"// test43.tig    
                    /* initialize with unit and causing type mismatch in addition */

                    let 
	                    var a := ()
                    in
	                    a + 3
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.NilTest () =
        let str = @"// test44.tig    
                    /* valid nil initialization and assignment */
                    let 

	                    type rectype = {name:string, id:int}
	                    var b:rectype := nil

                    in

	                    b := nil

                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.NilFailTest () =
        let str = @"// test45.tig    
                    /* error: initializing nil expressions not constrained by record type */
                    let 
	                    type rectype = {name:string, id:int}

	                    var a:= nil
                    in
	                    a
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.ValidRecordCompare () =
        let str = @"// test46.tig    
                    /* valid rec comparisons */
                    let 
	                    type rectype = {name:string, id:int}
	                    var b:rectype := nil
                    in
	                    b = nil;
	                    b <> nil
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)
    
    [<Test>]
    member self.TypeHiding2 () =
        let str = @"// test47.tig    
                    /* This is legal.  The second type ""a"" simply hides the first one.
                       Because of the intervening variable declaration, the two ""a"" types
                       are not in the same  batch of mutually recursive types.
                       See also test38 */
                    let
	                    type a = int
	                    var b := 4
	                    type a = string
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.FunctionHiding2 () =
        let str = @"// test48.tig    
                    /* This is legal.  The second type ""a"" simply hides the first one.
                       Because of the intervening variable declaration, the two ""a"" types
                       are not in the same  batch of mutually recursive types.
                       See also test38 */
                    let
	                    type a = int
	                    var b := 4
	                    type a = string
                    in
	                    0
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal (Some 1)

    [<Test>]
    member self.NilBeforeTypeId () =
        let str = @"// test48.tig    
                    /* error: syntax error, nil should not be preceded by type-id.  */
                    let 
	                    type rectype = {name:string, id:int}

	                    var a:= rectype nil
                    in
	                    a
                    end"
                           
        let parseResult = run prog str
        resultType parseResult |> should equal None

    [<Test>]
    member self.Queens () =
        let str = @"// queens.tig    
                    /* A program to solve the 8-queens problem */

                    let
                        var N := 8

                        type intArray = array of int

                        var row := intArray [ N ] of 0
                        var col := intArray [ N ] of 0
                        var diag1 := intArray [N+N-1] of 0
                        var diag2 := intArray [N+N-1] of 0

                        function printboard() =
                           (for i := 0 to N-1
	                     do (for j := 0 to N-1 
	                          do print(if col[i]=j then "" O"" else "" ."");
	                         print(""\n""));
                             print(""\n""))

                        function try(c:int) = 
                    ( /*  for i:= 0 to c do print("".""); print(""\n""); flush();*/
                         if c=N
                         then printboard()
                         else for r := 0 to N-1
	                       do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
	                               then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
		                             col[c]:=r;
	                                     try(c+1);
			                     row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)

                    )
                     in try(0)
                    end
                    "
                           
        let parseResult = run prog str
        resultType parseResult |> should equal None

    [<Test>]
    member self.Merge () =
        let str = @"// merge.tig    
                   let 

                     type any = {any : int}
                     var buffer := getchar()

                    function readint(any: any) : int =
                     let var i := 0
                         function isdigit(s : string) : int = 
		                      ord(buffer)>=ord(""0"") & ord(buffer)<=ord(""9"")
                         function skipto() =
                           while buffer="" "" | buffer=""\n""
                             do buffer := getchar()
                      in skipto();
                         any.any := isdigit(buffer);
                         while isdigit(buffer)
                           do (i := i*10+ord(buffer)-ord(""0""); buffer := getchar());
                         i
                     end

                     type list = {first: int, rest: list}

                     function readlist() : list =
                        let var any := any{any=0}
                            var i := readint(any)
                         in if any.any
                             then list{first=i,rest=readlist()}
                             else nil
                        end

                     function merge(a: list, b: list) : list =
                       if a=nil then b
                       else if b=nil then a
                       else if a.first < b.first 
                          then list{first=a.first,rest=merge(a.rest,b)}
                          else list{first=b.first,rest=merge(a,b.rest)}

                     function printint(i: int) =
                      let function f(i:int) = if i>0 
	                         then (f(i/10); print(chr(i-i/10*10+ord(""0""))))
                       in if i<0 then (print(""-""); f(-i))
                          else if i>0 then f(i)
                          else print(""0"")
                      end

                     function printlist(l: list) =
                       if l=nil then print(""\n"")
                       else (printint(l.first); print("" ""); printlist(l.rest))

                       var list1 := readlist()
                       var list2 := (buffer:=getchar(); readlist())


                      /* BODY OF MAIN PROGRAM */
                     in printlist(merge(list1,list2))
                    end
                    "
                           
        let parseResult = run prog str
        resultType parseResult |> should equal None

