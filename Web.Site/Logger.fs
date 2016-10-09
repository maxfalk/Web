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
        msg |> formatLogMsg level |> printfn "%s"  

    interface Logger with 
        member x.Log level line =
            (line()).message |> logToScreen (level.ToString())  |> ignore
            (line()).message |> logToFile (level.ToString())  |> ignore