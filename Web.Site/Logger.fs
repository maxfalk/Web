namespace WebLogger
open Suave.Logging
open FileWriter
open System
open System.IO

type WebLogger() = 
    let fileWriter = new FileWriter()
    let baseFileName = Directory.GetCurrentDirectory() + "/Logs/log"
    let getFileName() = 
        let now = DateTime.Now
        baseFileName + "_" + now.ToString("yyyyMMdd") + ".txt"
    let formatLogMsg level msg =
        "[" + System.DateTime.Now.ToString() + "]" +
        level + ": " +
        " " + msg +
        "\r\n"

    let logToFile level file msg =
        msg |> formatLogMsg level |> fileWriter.Write file
    
    let logToScreen level msg =
        msg |> formatLogMsg level |> printf "%s"  

    interface Logger with 
        member x.Log level line =
            let logLine = line()
            let file = getFileName()
            if level.ToInt() >= LogLevel.Warn.ToInt() then     
                logLine.message |> logToScreen (level.ToString())  |> ignore
            logLine.message |> logToFile (level.ToString())  file |> ignore