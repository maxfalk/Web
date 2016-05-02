module NetAreas

open FSharp.Data
open Point
open XMLEncode
open JsonEncode

type private Polygon = Point List

type private MultiPolygon = Polygon List

type Shape = 
    | Polygon of Polygon
    | MultiPolygon of MultiPolygon

type NetArea = 
    { Coordinates : Shape List
      Center : Point Option
      NetAreaID : string option
      NetAreaName : string option
      Company : string option }

let rec private isPointInPolygonHelper ((xp, yp) : Point) (polygon : Polygon) (windingNumber : float) = 
    match polygon with
    | [] -> windingNumber
    | [ h ] -> windingNumber
    | (x1, y1) :: (x2, y2) :: tl -> 
        if y1 * y2 < 0.0 then 
            let r = x1 + ((y1 * (x2 - x1)) / (y1 - y2))
            if r > 0.0 then 
                if (y1 < 0.0) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 1.0)
                else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 1.0)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) windingNumber
        else if (y1 = 0.0 && x1 > 0.0) then 
            if (y2 > 0.0) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 0.5)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 0.5)
        else if (y2 = 0.0 && x2 > 0.0) then 
            if (y1 < 0.0) then isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber + 0.5)
            else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) (windingNumber - 0.5)
        else isPointInPolygonHelper (xp, yp) ((x2, y2) :: tl) windingNumber

let private isPointInPolygon ((xp, yp) : Point) (polygon : Polygon) = 
    let posList = polygon |> List.map (fun (x, y) -> (x - xp, y - yp))
    ((isPointInPolygonHelper (xp, yp) posList 0.0) <> 0.0)

let private isPointInShape (point : Point) (shape : Shape) = 
    match shape with
    | Shape.Polygon(value) -> isPointInPolygon point value
    | Shape.MultiPolygon(values) -> List.fold (fun acc item -> acc && (isPointInPolygon point item)) true values

let private getPolygon (json : JsonValue) : Shape = 
    Shape.Polygon [ for x in json do
                        yield ((x.Item 0).AsFloat(), (x.Item 1).AsFloat()) ]

let private getMultiPolygon (json : JsonValue) : Shape = 
    Shape.MultiPolygon [ for x in json do
                             yield [ for y in x do
                                         yield ((y.Item 0).AsFloat(), (y.Item 1).AsFloat()) ] ]

let private getCoordsFromJson (json : JsonValue) : Shape List = 
    match (json.TryGetProperty "simple") with
    | Some(jsonSimple) -> 
        match jsonSimple.TryGetProperty "type" with
        | Some(jsonType) -> 
            match jsonType.AsString() with
            | "Polygon" -> 
                match (jsonSimple.TryGetProperty "coordinates") with
                | Some(jsonCoords) -> 
                    [ for x in jsonCoords do
                          yield getPolygon x ]
                | None -> []
            | "MultiPolygon" -> 
                match (jsonSimple.TryGetProperty "coordinates") with
                | Some(jsonCoords) -> 
                    [ for x in jsonCoords do
                          yield getMultiPolygon x ]
                | None -> []
            | _ -> []
        | None -> []
    | None -> []

let private getCenterFromJson (json : JsonValue) : Point Option = 
    match (json.TryGetProperty "center") with
    | Some(jsonCenter) -> 
        match (jsonCenter.TryGetProperty "coordinates") with
        | Some(jsonCoords) -> Some(((jsonCoords.Item 0).AsFloat(), (jsonCoords.Item 1).AsFloat()))
        | None -> None
    | None -> None

let private getStringFromJson (json : JsonValue) value = 
    match json.TryGetProperty value with
    | Some(x) -> Some(x.AsString())
    | None -> None

let private getNetArea json = 
    { Coordinates = (getCoordsFromJson json)
      Center = (getCenterFromJson json)
      NetAreaID = (getStringFromJson json "omr")
      NetAreaName = (getStringFromJson json "namn")
      Company = (getStringFromJson json "bolag") }

let getNetAreas jsonStr = 
    let jsonArray = (JsonValue.Parse jsonStr)
    [ for x in jsonArray do
          yield getNetArea (x.Item 0) ]

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
    + OptionValueToJson "ID" value.NetAreaID + ","
    + OptionValueToJson "Name" value.NetAreaName + ","
    + OptionValueToJson "Company" value.Company
    + "}"    
        

let NetAreaToXML(netArea : NetArea Option) =
    match netArea with
        | None -> ""
        | Some(value) ->
            MakeXMLTagFromOption "ID" value.Company
            + MakeXMLTagFromOption "Name" value.NetAreaName
            + MakeXMLTagFromOption "Company" value.Company

            


let NetAreaListToJson(netareaList : NetArea option List) = 
    let json = 
        List.fold (fun acc elm -> 
            (if acc = "" then NetAreaToJson elm
             else acc + "," + NetAreaToJson elm)) "" netareaList
    "{\"NetAreas\" : [" + json + "]}"
