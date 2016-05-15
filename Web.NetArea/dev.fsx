#r "../packages/FSharp.Data.2.3.0/lib/net40/FSharp.Data.dll"
#load "Point.fs"
open FSharp.Data
open Point

type Polygon = Point List
type MultiPolygon = Polygon List
type Shape = 
    | Polygon of Polygon
    | MultiPolygon of MultiPolygon

type NetArea = 
    { Coordinates : Shape List
      Center : Point Option
      NetAreaID : string option
      NetAreaName : string option
      Company : string option }


type NetAreas = JsonProvider<"jsonData.json">
let netAreas = NetAreas.GetSamples()

let printPolygon (item:decimal[][]) =
    printfn "Coords: "
    for x in item do 
        printfn "[%f, %f]" (x.[0]) (x.[1]) 


type JsonAddressResult = JsonProvider<"https://maps.googleapis.com/maps/api/geocode/json?address=Kungliga Slottet 07 70 Stockholm&key=AIzaSyALKUXasyH_3ktBiHBD54PwsWHLN6RFC58">
let  addr = JsonAddressResult.GetSample()
addr.Results


type Agent<'T> = MailboxProcessor<'T>
 
let agent =
   Agent.Start(fun inbox ->
     async { while true do
               let! msg = inbox.Receive()
               printfn "got message '%s'" msg } )


agent.Post "hello!"