module Address

open System.Net
open FSharp.Data
open System
open Point
open JsonEncode

type AddressComponent = 
    { LongName : string option
      ShortName : string option
      Type : string }

type Address = 
    { Address : string option
      AddressNumber : string option
      ZipCode : string option
      City : string option
      Country : string option
      FormattedAddress : string option
      Location : Point option }

let FindSome sequence = 
    if sequence |> Seq.exists Option.isSome then sequence |> Seq.find Option.isSome
    else None


let JsonTryGet (json : JsonValue) obj f = json.TryGetProperty obj |> Option.bind f
let JsonTryGetFloat (json : JsonValue) obj = JsonTryGet json obj (fun v -> Some(v.AsFloat()))
let JsonTryGetString (json : JsonValue) obj = JsonTryGet json obj (fun v -> Some(v.AsString()))

let GetRightValue (addrtype : JsonValue) addressType value = 
    if addrtype.AsString() = addressType then 
        Some({ LongName = JsonTryGetString value "long_name"
               ShortName = JsonTryGetString value "short_name"
               Type = addressType })
    else None

let ParseAddressComponent (json : JsonValue) (addressType : string) = 
    match json.TryGetProperty "types" with
    | Some(value) -> 
        let valueList = value.AsArray() |> Seq.map (fun x -> GetRightValue x addressType json)
        valueList |> FindSome
    | None -> None

let ParseAddressComponents (json : JsonValue) (jsonObject : string) = 
    match json.TryGetProperty "address_components" with
    | Some(value) -> 
        let matches = 
            [ for item in value do
                  yield ParseAddressComponent item jsonObject ]
        matches |> FindSome
    | None -> None

let ParseAddressJson(json : JsonValue) : Address list = 
    match json.TryGetProperty "status" with
    | Some(value) -> 
        match value.AsString() with
        | "OK" -> 
            let result = json.TryGetProperty "results"
            [ for x in result.Value do
                  yield { Address = (ParseAddressComponents x "route") |> Option.bind (fun v -> v.LongName)
                          ZipCode = (ParseAddressComponents x "postal_code") |> Option.bind (fun v -> v.LongName)
                          City = (ParseAddressComponents x "locality") |> Option.bind (fun v -> v.LongName)
                          Country = (ParseAddressComponents x "country") |> Option.bind (fun v -> v.LongName)
                          AddressNumber = 
                              (ParseAddressComponents x "street_number") |> Option.bind (fun v -> v.LongName)
                          FormattedAddress = JsonTryGetString x "formatted_address"
                          Location = 
                              (x.TryGetProperty "geometry")
                              |> Option.bind (fun geo -> geo.TryGetProperty "location")
                              |> Option.bind 
                                     (fun loc -> 
                                     Some((loc.GetProperty "lng").AsFloat(), ((loc.GetProperty "lat").AsFloat()))) } ]
        | "ZERO_RESULTS" -> []
        | "OVER_QUERY_LIMIT" -> []
        | "REQUEST_DENIED" -> []
        | "INVALID_REQUEST" -> []
        | "UNKNOWN_ERROR" -> []
        | _ -> []
    | None -> []

let AddressToJson(address : Address) = 
    let (lat, lng) = PointToString address.Location.Value
    "{" + OptionStringToJson "Address" address.Address + "," + OptionStringToJson "AddressNumber" address.AddressNumber 
    + "," + OptionStringToJson "Zipcode" address.ZipCode + "," + OptionStringToJson "City" address.City + "," 
    + OptionStringToJson "Country" address.Country + "," 
    + OptionStringToJson "Formatted Address" address.FormattedAddress + "," + StringToJson "Longitude" lng + "," 
    + StringToJson "Latitude" lat + "}"

let AddressListToJson(addressList : Address list) = 
    let addressJsonString = 
        addressList |> List.fold (fun acc elm -> 
                           if acc = "" then AddressToJson elm
                           else acc + "," + AddressToJson elm) ""
    "{\"Addresses\" : [" + addressJsonString + "]}"

let GetAddressPostions address = 
    async { 
        let apiKey = "AIzaSyALKUXasyH_3ktBiHBD54PwsWHLN6RFC58"
        let geoUrl = "https://maps.googleapis.com/maps/api/geocode/json?address=" + address + "&key=" + apiKey
        let client = new WebClient()
        let! data = client.AsyncDownloadString(new Uri(geoUrl))
        let json = JsonValue.Parse data
        return ParseAddressJson(json)
    }
