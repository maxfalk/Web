module Program

open System.IO
open System.Text
open System.Net
open NetAreas
open Address
open Point
open XMLEncode

open Suave
open Suave.Http
open Suave.Filters
open Suave.Successful
open Suave.Web
open Suave.Operators
open Suave.Files

let IsSuccess value =
    match value with
        | "OK" -> "1"
        | _ -> "0"

let GetErrorCode value =
    match value with
        | "OK" -> "0"
        | "ZERO_RESULTS" -> "2"
        | "OVER_QUERY_LIMIT" -> "3"
        | "REQUEST_DENIED" -> "4"
        | "INVALID_REQUEST" -> "5"
        | "UNKNOWN_ERROR" -> "98"
        | _ -> "99"

let GetErrorString value =
    match value with
        | "OK" -> "\"\""
        | errorString -> errorString

let GetNetAreaOfAddressToXML address netAreas = 
    let (result, addresses) = GetAddressPostions address |> Async.RunSynchronously   
    let matches = addresses |> List.fold (fun acc elm -> MakeXMLTag "Record" (MakeXMLTag "NetArea" (NetAreaToXML (isInWhichNetArea elm.Location.Value netAreas)) + MakeXMLTag "Address" (AddressToXML elm)) + acc) ""
    let success = MakeXMLTag "Success" (IsSuccess result)
    let errorCode = MakeXMLTag "ErrorCode" (GetErrorCode result)
    let errorString = MakeXMLTag "ErrorString" (GetErrorString result)
    let records = (MakeXMLTagWithAttribute "Result" matches "recordes" (addresses.Length.ToString()))
    MakeXMLTag "root" ( success + errorCode + errorString + records)
     



let GetNetAreaOfAddressToJson address netAreas = 
    let (result, addresses) = GetAddressPostions address |> Async.RunSynchronously   
    let matches = 
        addresses 
            |> List.fold (fun acc elm ->
                if acc = "" then
                    "{"
                    + "\"NetArea\" : "
                    + NetAreaToJson (isInWhichNetArea elm.Location.Value netAreas)
                    + ", \"Address\" : "
                    + AddressToJson elm
                    + "}"    
                else 
                    "{"
                    + "\"NetArea\" : "
                    + NetAreaToJson (isInWhichNetArea elm.Location.Value netAreas)
                    + ", \"Address\" : "
                    + AddressToJson elm + "}, " + acc) ""
    "{"
     + "\"Success\" : "
     + IsSuccess result
     + ", \"ErrorCode\" : "
     + GetErrorCode result
     + ", \"ErrorString\" : "
     + GetErrorString result
     + ",\"Result\" : ["
      + matches + "]}"

let loadNetAreas() = 
    try 
        let jsonStr = File.ReadAllText(@"jsonData.txt", Encoding.UTF8)
        getNetAreas jsonStr
    with
    | :? System.ArgumentException as ex -> 
        printfn "%s" ex.Message
        []
    | :? System.IO.IOException as ex -> 
        printfn "%s" ex.Message
        []
    | :? System.UnauthorizedAccessException as ex -> 
        printfn "%s" ex.Message
        []
    | :? System.NotSupportedException as ex -> 
        printfn "%s" ex.Message
        []
    | :? System.Security.SecurityException as ex -> 
        printfn "%s" ex.Message
        []

let serverConfig = 
    { defaultConfig with logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Verbose
                         bindings = [ HttpBinding.mk HTTP IPAddress.Loopback 8082us ] }

[<EntryPoint>]
let main argv = 
    let netAreas = loadNetAreas()   
    let app : WebPart = 
        choose [ choose [ pathScan "/API/json/netarea/address/%s" (fun addr ->
                                                              OK((GetNetAreaOfAddressToJson addr netAreas))) 
                          >=> Writers.setMimeType "application/json; charset=utf-8"
                          pathScan "/API/xml/netarea/address/%s" (fun addr ->
                                                                 OK((GetNetAreaOfAddressToXML addr netAreas))) 
                          >=> Writers.setMimeType "application/json; charset=utf-8"
                          path "/" >=> file "web/index.html"
                          pathScan "web/css/%s" (fun addr -> file ("css/" + addr))
                          pathScan "/%s" (fun addr -> file ("web/" + addr))
                          pathScan "web/images/%s" (fun addr -> file ("images/" + addr)) ] ]
    startWebServer serverConfig app
    0
