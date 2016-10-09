module FileWriter
open System.IO
type FileWirterMsg = Msg of string | EXIT

type FileWriter(file) =
    let writerAgent file = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop file =
                async{
                    let! msg = inbox.Receive()
                    match msg with
                    | Msg(value) ->
                        File.AppendAllText(file, value) 
                        return! loop file
                    | EXIT ->
                        ()
                }
            loop file 
        )      
    let writer = writerAgent file
    member this.Write text = 
        writer.Post (FileWirterMsg.Msg text)
    member this.Close() = 
        writer.Post FileWirterMsg.EXIT


