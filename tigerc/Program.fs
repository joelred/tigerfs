// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Tiger
open NDesk.Options

[<EntryPoint>]
let main argv = 
    let compiler = new TigerCompiler ()
    
    let options =
         (new OptionSet ())    
            .Add( "a|printAst", (fun v -> compiler.DumpSyntaxTree <- true))
            .Add( "d|printDesugaredTree", (fun v -> compiler.DumpDesugaredTree <- true))

    let filePaths = List.ofSeq (options.Parse argv)

    List.iter    
        (fun path -> compiler.CompileFile path)
        filePaths

    
    0
