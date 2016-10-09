module Address

open System.Text
open System.Net
open FSharp.Data
open System
open Point
open JsonEncode
open XMLEncode

[<Literal>]
//let JsonProviderString = "https://maps.googleapis.com/maps/api/geocode/json?address=Kungliga Slottet 07 70 Stockholm&key=AIzaSyALKUXasyH_3ktBiHBD54PwsWHLN6RFC58"
let JsonProviderString = "googleAPI.json"
type JsonAddressResult = JsonProvider<JsonProviderString>

type AddressComponent = 
    { LongName : string
      ShortName : string
      Type : string }

type Address = 
    { Address : string option
      AddressNumber : string option
      ZipCode : string option
      City : string option
      Country : string option
      FormattedAddress : string
      Location : Point option }

let FindSome sequence = 
    if sequence |> Seq.exists Option.isSome then sequence |> Seq.find Option.isSome
    else None


let JsonTryGet (json : JsonValue) obj f = json.TryGetProperty obj |> Option.bind f
let JsonTryGetFloat (json : JsonValue) obj = JsonTryGet json obj (fun v -> Some(v.AsFloat()))
let JsonTryGetString (json : JsonValue) obj = JsonTryGet json obj (fun v -> Some(v.AsString()))

let GetRightValue addrtype addressType (value : JsonAddressResult.AddressComponent) = 
    if addrtype = addressType then 
        Some({ LongName = value.LongName
               ShortName = value.ShortName
               Type = addressType })
    else None

let ParseAddressComponent (json : JsonAddressResult.AddressComponent) (addressType : string) = 
    let valueList = json.Types |> Seq.map (fun x -> GetRightValue x addressType json)
    valueList |> FindSome

let ParseAddressComponents (json : JsonAddressResult.AddressComponent[]) (jsonObject : string) = 
    let matches = 
        [ for item in json do
                yield ParseAddressComponent item jsonObject ]
    matches |> FindSome

let ParseAddressJson(json : JsonAddressResult.Root) : Address list = 
    [ for x in json.Results do
      yield { Address = (ParseAddressComponents x.AddressComponents "route") |> Option.bind (fun v -> Some v.LongName)
              ZipCode = (ParseAddressComponents x.AddressComponents "postal_code") |> Option.bind (fun v -> Some v.LongName)
              City = (ParseAddressComponents x.AddressComponents "locality") |> Option.bind (fun v -> Some v.LongName)
              Country = (ParseAddressComponents x.AddressComponents "country") |> Option.bind (fun v -> Some v.LongName)
              AddressNumber = (ParseAddressComponents x.AddressComponents "street_number") |> Option.bind (fun v -> Some v.LongName)
              FormattedAddress = x.FormattedAddress
              Location = Some (x.Geometry.Location.Lng, x.Geometry.Location.Lat)}]


let ParseResult (json : JsonAddressResult.Root) =
 match json.Status with
    | "OK" -> ("OK", ParseAddressJson json)
    | "ZERO_RESULTS" -> ("ZERO_RESULTS", [])
    | "OVER_QUERY_LIMIT" -> ("OVER_QUERY_LIMIT", [])
    | "REQUEST_DENIED" -> ("REQUEST_DENIED", [])
    | "INVALID_REQUEST" -> ("INVALID_REQUEST", [])
    | "UNKNOWN_ERROR" -> ("UNKNOWN_ERROR", [])
    | _ -> ("UNMATCHED_ERROR", [])
   
let AddressToJson(address : Address) = 
    let (lat, lng) = PointToString address.Location.Value
    "{" + OptionStringToJson "Address" address.Address + ","
    + OptionStringToJson "AddressNumber" address.AddressNumber  + ","
    + OptionStringToJson "Zipcode" address.ZipCode + ","
    + OptionStringToJson "City" address.City + "," 
    + OptionStringToJson "Country" address.Country + "," 
    + StringToJson "Formatted Address" address.FormattedAddress + ","
    + ValueToJson "Longitude" lng + "," 
    + ValueToJson "Latitude" lat + "}"

let AddressListToJson(addressList : Address list) = 
    let addressJsonString = 
        addressList |> List.fold (fun acc elm -> 
                           if acc = "" then AddressToJson elm
                           else acc + "," + AddressToJson elm) ""
    "{\"Addresses\" : [" + addressJsonString + "]}"

let AddressToXML(address : Address) =
    let (lat, lng) = PointToString address.Location.Value
    MakeXMLTagFromOption "Address" address.Address +
    MakeXMLTagFromOption "AddressNumber" address.AddressNumber +
    MakeXMLTagFromOption "Zipcode" address.ZipCode +
    MakeXMLTagFromOption "City" address.City +
    MakeXMLTagFromOption "Country" address.Country +
    MakeXMLTag "FormattedAddress" address.FormattedAddress +
    MakeXMLTag "Longitude" lng +
    MakeXMLTag "Latitude" lat
               

let GetAddressPostions address = 
    async { 
        let apiKey = "AIzaSyALKUXasyH_3ktBiHBD54PwsWHLN6RFC58"
        let geoUrl = "https://maps.googleapis.com/maps/api/geocode/json?address=" + address + "&key=" + apiKey
        let client = new WebClient()
        let! data = client.AsyncDownloadString(new Uri(geoUrl))
        let s = Encoding.UTF8.GetString(Encoding.Default.GetBytes(data))
        let json = JsonAddressResult.Parse s
        return ParseResult json
    }