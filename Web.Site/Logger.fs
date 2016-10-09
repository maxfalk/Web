namespace WebLogger
open Suave.Logging


type WebLogger() = 
    interface Logger with 
        member x.Log level line =
            match level with
                | LogLevel.Fatal -> printfn "Fatal Error: %s" (line()).message
                | _ -> printfn "%s" (line()).message
