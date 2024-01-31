---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON-hantering är processen att tolka och skriva JSON (JavaScript Object Notation), ett lättviktigt datautbytesformat. Programmerare använder JSON för att enkelt överföra data mellan server och klient eller mellan applikationens moduler.

## How to:
I Swift kan du använda `JSONSerialization` för att hantera JSON. För att konvertera JSON-sträng till Swift-objekt: 

```Swift
import Foundation

let jsonString = """
{
    "name": "Anna",
    "age": 29,
    "isDeveloper": true
}
"""

if let jsonData = jsonString.data(using: .utf8) {
    do {
        let jsonObject = try JSONSerialization.jsonObject(with: jsonData, options: [])
        print(jsonObject)
    } catch {
        print(error)
    }
}
```

För att skapa JSON-sträng från ett Swift-objekt:

```Swift
let userDictionary: [String: Any] = [
    "name": "Erik",
    "age": 35,
    "isDeveloper": false
]

do {
    let jsonData = try JSONSerialization.data(withJSONObject: userDictionary, options: [])
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString)
    }
} catch {
    print(error)
}
```

## Deep Dive
JSON introducerades i början av 2000-talet och har blivit en standard för webbkommunikation. JSON är lättare än XML och fungerar väl med JavaScript, vilket gör det populärt för webbutvecklare. Swift erbjuder också `Codable` protokollet för enklare (de)serialisering som ett alternativ till `JSONSerialization`. Codable är mer typsäkert och rekommenderas för de flesta JSON-uppgifter.

## See Also
- [Apple's JSON and Codable Guide](https://developer.apple.com/documentation/foundation/jsonencoder)
- [JSON.org](http://json.org/)
- [W3Schools JSON Introduction](https://www.w3schools.com/js/js_json_intro.asp)
