
#I "../packages/FSharp.Charting.0.90.13"
#r "bin/FSharp.Data.dll"
#I "../packages/FSharp.Data.2.2.5"
#load "FSharp.Charting.fsx"
#r "bin/FSharp.Data.DesignTime.dll"
#r "bin/Suave.dll"


open System.Net;
open FSharp.Data;
open System.Drawing;
open System.Windows.Forms;
open FSharp.Charting;
open System.IO;
open Suave.Json;
open System.Text;

open System.Net
open FSharp.Data
open System;
type Point = float * float
type AddressComponent = {LongName: string option;
                         ShortName: string option;
                         Type: string;}

type Address = {Address: string option;
                AddressNumber:string option;
                ZipCode: string option;
                City: string option;
                Country: string option;
                FormattedAddress: string option;
                Location: Point option;}


let FindSome sequence =
    if sequence |> Seq.exists (fun x -> Option.isSome x) then
        sequence |> Seq.find (fun x -> Option.isSome x)
    else
        None

let JsonTryGet (json:JsonValue) obj f =
    json.TryGetProperty obj |> Option.bind f

let JsonTryGetFloat (json:JsonValue) obj =
    JsonTryGet json obj (fun v -> Some(v.AsFloat()))

let JsonTryGetString (json:JsonValue) obj =
    JsonTryGet json obj  (fun v -> Some(v.AsString()))

let GetRightValue (addrtype:JsonValue) addressType value =
    if addrtype.AsString() = addressType then
        Some({ LongName = JsonTryGetString value "long_name";
               ShortName = JsonTryGetString value"short_name";
               Type = addressType})
    else
        None

let ParseAddressComponent (json:JsonValue) (addressType:string)  =
    match json.TryGetProperty "types" with
        | Some(value) ->
            let valueList = value.AsArray() |> Seq.map (fun x -> GetRightValue x addressType json)
            valueList |> FindSome            
        | None ->
            None


let ParseAddressComponents (json:JsonValue) (jsonObject:string) =
    match json.TryGetProperty "address_components" with
        | Some(value) ->
            let matches = [for item in value do yield ParseAddressComponent item jsonObject]
            matches |> FindSome            
        | None ->
            None
 
let ParseAddressJson (json:JsonValue) : Address list  =
    match json.TryGetProperty "status" with
        | Some(value) ->
            match value.AsString() with
                | "OK" ->
                    let result = json.TryGetProperty "results"
                    [ for x in result.Value do  
                      yield {Address = (ParseAddressComponents x "route")
                             |> Option.bind (fun v -> v.LongName);
                             ZipCode = (ParseAddressComponents x "postal_code")
                             |> Option.bind (fun v -> v.LongName);
                             City = (ParseAddressComponents x "locality")
                             |> Option.bind (fun v -> v.LongName);
                             Country = (ParseAddressComponents x "country")
                             |> Option.bind (fun v -> v.LongName);
                             AddressNumber = (ParseAddressComponents x "street_number")
                             |> Option.bind (fun v -> v.LongName);
                             FormattedAddress = JsonTryGetString x "formatted_address";
                             Location = (x.TryGetProperty "geometry")
                                         |> Option.bind (fun geo -> geo.TryGetProperty "location")
                                         |> Option.bind (fun loc -> Some((loc.GetProperty "lng").AsFloat(),
                                                                         ((loc.GetProperty "lat").AsFloat())))}]
                | _ ->
                    []
        | None ->
            []




let gdata = File.ReadAllText "/home/max/projects/ElomradeAPI/ElomradeAPI/tesdata.json"
let gjson = JsonValue.Parse gdata
let gparsed =  ParseAddressJson(gjson)

Encoding.UTF8.GetString (toJson gparsed)

gparsed
