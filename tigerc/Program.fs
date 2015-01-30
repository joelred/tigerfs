// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Tiger

[<EntryPoint>]
let main argv = 
    let compiler = new TigerCompiler ()
    
    compiler.CompileFile argv.[0]
    
    0
