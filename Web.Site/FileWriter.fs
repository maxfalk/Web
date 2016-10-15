module FileWriter
open System.IO
open Agent
type FileWirterMsg = Msg of string*string | EXIT

type FileWriter() =
    let writerAgent file = 
        Agent.Start(fun inbox ->
            let rec loop() =
                async{
                    let! msg = inbox.Receive()
                    match msg with
                    | Msg(file, value) ->
                        File.AppendAllText(file, value) 
                        return! loop()
                    | EXIT ->
                        ()
                }
            loop() 
        )      
    let writer = writerAgent()
    member this.Write file text = 
        writer.Post (FileWirterMsg.Msg (file,text))
    member this.Close() = 
        writer.Post FileWirterMsg.EXIT


