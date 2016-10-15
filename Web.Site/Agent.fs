module Agent

type Agent<'T> = MailboxProcessor<'T>

type MailboxProcessor<'T> with
    static member Supervisor supervised arg =
        MailboxProcessor.Start(fun inbox ->
            let rec loop() = async{
                try 
                    printfn "Supervisor starting func"
                    supervised arg
                    return! loop()
                with ex ->
                    return! loop()
                }
            loop())