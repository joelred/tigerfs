module internal Tiger.Translate

open ErrorMsgs

type Exp =
    unit

type Access = Level * Frames.Access

and LevelType = 
        { Parent: Level;
          Frame : Frames.Frame }

and Level =
    | Outer
    | Inner of LevelType

let Outermost = Outer

// We add an extra "parameter" to hold the static link
let NewLevel parent name formals =
    Inner { Parent = parent;
             Frame = Frames.newFrame (name, true::formals) }

let Formals level = function 
    | Outer -> []
    | Inner level -> level.Frame.Formals

let AllocateLocal level escapes = 
    match level with
    | Outer -> ErrorMsg.Impossible "Attempt to allocate local var in outer level"
    | Inner nest -> (level, nest.Frame.AllocLocal escapes)

