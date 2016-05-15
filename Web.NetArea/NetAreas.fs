module NetAreas

open System.IO
open System.Text
open Domain
open FSharp.Data
open Point
open XMLEncode
open JsonEncode

[<Literal>]
let JsonProviderFile = "jsonData.json"

type NetAreasJsonResult = JsonProvider<JsonProviderFile>
type private Polygon = Point List
type private MultiPolygon = Polygon List

type Shape = 
    | Polygon of Polygon
    | MultiPolygon of MultiPolygon

type NetArea = 
    { Coordinates : Shape List
      Center : Point Option
      NetAreaID : string 
      NetAreaName : string 
      Company : string  }

let rec private isPointInPolygonHelper ((xp, yp) : Point) (polygon : Polygon) (windingNumber : decimal) = 
    match polygon with
    | [] -> windingNumber
    | [h] -> windingNumber
    | (x1, y1) :: (x2, y2) :: tl -> 
        if y1 * y2 < 0.0m then 
            let r = x1 + ((y1 * (x2 - x1)) / (y1 - y2))
            if r > 0.0m then 
                if (y1 < 0.0m) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 1.0m)
                else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 1.0m)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) windingNumber
        else if (y1 = 0.0m && x1 > 0.0m) then 
            if (y2 > 0.0m) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 0.5m)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 0.5m)
        else if (y2 = 0.0m && x2 > 0.0m) then 
            if (y1 < 0.0m) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 0.5m)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 0.5m)
        else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) windingNumber

let private isPointInPolygon ((xp, yp) : Point) (polygon : Polygon) = 
    let posList = polygon |> List.map (fun (x, y) -> (x - xp, y - yp))
    ((isPointInPolygonHelper (xp, yp) posList 0.0m) <> 0.0m)

let private isPointInShape (point : Point) (shape : Shape) = 
    match shape with
    | Shape.Polygon(value) -> isPointInPolygon point value
    | Shape.MultiPolygon(values) -> List.fold (fun acc item -> acc || (isPointInPolygon point item)) false values

let private getPolygon (json : NetAreasJsonResult.NumbersOrArrays[]) : Polygon = 
    [ for x in json do yield (x.Numbers.[0], x.Numbers.[1]) ]

let private getMultiPolygon (json : NetAreasJsonResult.NumbersOrArrays[]) : MultiPolygon = 
     [ for x in json do yield [for y in x.Arrays do yield (y.[0], y.[1]) ]]

let private getCoordsFromNetArea (simple : NetAreasJsonResult.Simple) : Shape List = 
    match simple.Type with
        | "Polygon" -> 
            [ for x in simple.Coordinates do
                    yield Shape.Polygon (getPolygon x) ]
        | "MultiPolygon" -> 
            [ for x in simple.Coordinates do
                    yield Shape.MultiPolygon (getMultiPolygon x) ]
        | _ -> []


let private getPointFromCenter (center : NetAreasJsonResult.Center) : Point Option =    
    match center.Type with
    | "Point" ->        
        Some(((center.Coordinates).[0], center.Coordinates.[1]))
    | _ -> None

let private getStringFromJson (json : JsonValue) value = 
    match json.TryGetProperty value with
    | Some(x) -> Some(x.AsString())
    | None -> None

let private getNetArea (netArea:NetAreasJsonResult.Root) = 
    { Coordinates = getCoordsFromNetArea netArea.Simple
      Center = getPointFromCenter netArea.Center
      NetAreaID = netArea.Omr
      NetAreaName = netArea.Namn
      Company = netArea.Bolag }

let private getNetAreas jsonStr = 
    let json2Array = (NetAreasJsonResult.Parse jsonStr)
    [ for jsonArray in json2Array do
        for x in jsonArray do
          yield getNetArea x ]

let loadNetAreas() = 
    try 
        let jsonStr = File.ReadAllText(JsonProviderFile, Encoding.UTF8)
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


let rec isInWhichNetArea (point : Point) (netAreaList : NetArea List) = 
    match netAreaList with
    | h :: tl -> 
        let isInShape = h.Coordinates |> List.map (fun x -> isPointInShape point x)
        if isInShape |> List.exists id then Some(h)
        else isInWhichNetArea point tl
    | [] -> None

let NetAreaToJson(netarea : NetArea option) = 
    match netarea with
    | None -> "{}"
    | Some(value) ->
    "{" 
    + ValueToJson "ID" value.NetAreaID + ","
    + ValueToJson "Name" value.NetAreaName + ","
    + ValueToJson "Company" value.Company
    + "}"    
        

let NetAreaToXML(netArea : NetArea Option) =
    match netArea with
        | None -> ""
        | Some(value) ->
            MakeXMLTag "ID" value.NetAreaID
            + MakeXMLTag "Name" value.NetAreaName
            + MakeXMLTag "Company" value.Company

            
let NetAreaListToJson(netareaList : NetArea option List) = 
    let json = 
        List.fold (fun acc elm -> 
            (if acc = "" then NetAreaToJson elm
             else acc + "," + NetAreaToJson elm)) "" netareaList
    "{\"NetAreas\" : [" + json + "]}"
