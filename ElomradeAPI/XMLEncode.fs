module XMLEncode

let MakeXMLTag tag value =
    "<" + tag + ">" + value + "</" + tag + ">"

let MakeXMLTagFromOption tag stringValue =
    match stringValue with
        | Some(value) ->
            MakeXMLTag tag value
        | None ->
            MakeXMLTag tag ""

let MakeXMLTagWithAttribute tag value attr attrValue =
    "<" + tag + " " + attr + "=\"" + attrValue + "\"" + ">" + value + "</" + tag + ">"
