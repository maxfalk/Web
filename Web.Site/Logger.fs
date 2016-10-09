namespace WebLogger
open Suave.Logging
open FileWriter

type WebLogger() = 
    let fileWriter = new FileWriter("./log.txt")

    let formatLogMsg level msg =
        "[" + System.DateTime.Now.ToString() + "]" +
        level + ": " +
        " " + msg +
        "\r\n"

    let logToFile level msg =
        msg |> formatLogMsg level |> fileWriter.Write
    
    let logToScreen level msg =
        msg |> formatLogMsg level |> printf "%s"  

    interface Logger with 
        member x.Log level line =
            let logLine = line()
            if level >= LogLevel.Warn then
                logLine.message |> logToScreen (level.ToString())  |> ignore
            logLine.message |> logToFile (level.ToString())  |> ignore