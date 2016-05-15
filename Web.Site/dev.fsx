
#I "../packages/FSharp.Charting.0.90.13"
#r "bin/FSharp.Data.dll"
#I "../packages/FSharp.Data.2.2.5"
#load "FSharp.Charting.fsx"
#r "bin/FSharp.Data.DesignTime.dll"
#r "bin/Suave.dll"
#load "Point.fs"
#load "NetAreas.fs"
#load "JsonEncode.fs"
#load "Address.fs"

open System.Text
open System.Net
open FSharp.Data
open System
open Point
open JsonEncode
open NetAreas
open Address
open System.IO



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


let WhichNetareaIsAddressIn address netAreas = 
    let (result, addresses) = GetAddressPostions address |> Async.RunSynchronously   
    let matches = 
        addresses 
            |> List.fold (fun acc elm ->
                if acc = "" then
                     "\"NetArea\" : "
                    + NetAreaToJson (isInWhichNetArea elm.Location.Value netAreas)
                    + ", \"Address\" : "
                    + AddressToJson elm
                    + "}"    
                else 
                     "\"NetArea\" : "
                    + NetAreaToJson (isInWhichNetArea elm.Location.Value netAreas)
                    + ", \"Address\" : "
                    + AddressToJson elm + "}, " + acc) ""
    "{"
     + "{"
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

let net = loadNetAreas()

let res = WhichNetareaIsAddressIn "ringvägen 4 sverige" net
res;;

let gdata = File.ReadAllText("/home/max/projects/ElomradeAPI/ElomradeAPI/tesdata.json", Encoding.UTF8)
let gjson = JsonValue.Parse gdata
let gparsed =  ParseAddressJson(gjson)

Encoding.UTF8.GetString (toJson gparsed)

open System.Text
open System.Net
open FSharp.Data
open System.IO
open System

let address = "skördevägen 34 östersund"
let apiKey = "AIzaSyALKUXasyH_3ktBiHBD54PwsWHLN6RFC58"
let geoUrl = "https://maps.googleapis.com/maps/api/geocode/json?address=" + address + "&key=" + apiKey
let client = new WebClient()
client.Headers.Add(HttpRequestHeader.AcceptCharset, "UTF-8")
let data = client.DownloadData(new Uri(geoUrl))

let s = Encoding.UTF8.GetString(data)

s;;
