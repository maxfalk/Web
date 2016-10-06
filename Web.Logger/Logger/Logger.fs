namespace Logger
open Suave

type Logger() = 
    interface Logger with 
    member __.Log level line =
