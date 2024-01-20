---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Arbeid med JSON handler om å parse og generere strukturerte data. Programmerere trenger dette for å enkelt kommunisere med netttjenester og lagre kompleks data enkelt.

## How to:
```Swift
import Foundation

// JSON String
let jsonString = """
{
    "name": "Ola Nordmann",
    "age": 30,
    "isDeveloper": true
}
"""

// Konvertere JSON String til Dictionary
if let jsonData = jsonString.data(using: .utf8) {
    do {
        if let person = try JSONSerialization.jsonObject(with: jsonData) as? [String: Any] {
            print(person)
        }
    } catch {
        print("JSON parsing feil: \(error)")
    }
}

// Opprette og konvertere Dictionary til JSON String
let personDict: [String: Any] = [
    "name": "Kari Nordmann",
    "age": 28,
    "isDeveloper": false
]

if let jsonData = try? JSONSerialization.data(withJSONObject: personDict, options: []) {
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString)
    }
}
```

Sample output:
```
["name": "Ola Nordmann", "age": 30, "isDeveloper": true]
{"name":"Kari Nordmann","isDeveloper":false,"age":28}
```

## Deep Dive
JSON, JavaScript Object Notation, ble skapt tidlig på 2000-tallet og har blitt standard for dataformat på nettet. Alternativer inkluderer XML og YAML, men JSON er foretrukket for sin enkelhet. I Swift brukes `JSONSerialization` klassen for å parse og generere JSON, men Swift 4 introduserte `Codable`, en mer deklarativ måte å håndtere JSON.

## See Also
- Swift's `Codable` documentation: https://developer.apple.com/documentation/swift/codable
- JSON standard official site: https://www.json.org/json-en.html
- Apple's Networking with URLSession: https://developer.apple.com/documentation/foundation/urlsession