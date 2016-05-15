module JsonEncode


let ValueToString value =
   string value 

let ValueToJson tag value = 
    "\"" + tag + "\" : \"" + ValueToString value + "\""

let StringToJson tag (value:string) = 
    "\"" + tag + "\" : \"" +  value + "\""

       
let OptionValueToJson tag value =
    match value with
        | Some(v) ->
            ValueToJson tag v
        | None ->
            ValueToJson tag ""

let OptionStringToJson tag value =
    match value with
        | Some(v) ->
            StringToJson tag v
        | None ->
            StringToJson tag ""

let private JsonConcat values =
    let appendIfNotFirst acc value =
        if acc = "" then
            value
        else
            "," + value
    values |> List.fold appendIfNotFirst ""

let MakeJsonArray values =
    "\[" + JsonConcat values  + "\]"
