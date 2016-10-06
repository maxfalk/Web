namespace WebLogger
open Suave.Logging


type WebLogger() = 
    let logToScreen level line =
        line |> ignore

    interface Logger with 
        member __.Log level line =
            match level with
                | LogLevel.Fatal -> logToScreen level line
                | _ -> logToScreen level line