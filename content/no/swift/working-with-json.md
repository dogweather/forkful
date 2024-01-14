---
title:                "Swift: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å jobbe med JSON? JSON, som står for JavaScript Object Notation, er et populært format for å lagre og overføre data. Det brukes ofte i web-applikasjoner og mobilapplikasjoner for å kommunisere med servere og sikre at dataene som blir sendt og mottatt er i et strukturert og lesbart format.

## Hvordan

Å jobbe med JSON i Swift er enkelt og effektivt. Her er et eksempel på hvordan man kan konvertere data som er lagret i et JSON-format til et Swift-objekt og hente ut spesifikke verdier:

```Swift
if let jsonData = jsonString.data(using: .utf8) {
    let jsonObject = try? JSONSerialization.jsonObject(with: jsonData, options: [])
    if let jsonDictionary = jsonObject as? [String: Any],
        let name = jsonDictionary["name"] as? String,
        let age = jsonDictionary["age"] as? Int {
            print("\(name) is \(age) years old")
    }
}
```

I dette eksempelet lager vi først en`Data`-objekt fra den JSON-strengen vi ønsker å jobbe med. Deretter bruker vi`JSONSerialization` for å konvertere dette til et `Any`-objekt, som vi deretter cast'er til et `Dictionary` hvor vi kan hente ut spesifikke verdier ved hjelp av nøklene i JSON-strengen.

## Dypdykk

Det finnes flere måter å jobbe med JSON i Swift, og det er viktig å forstå forskjellene mellom dem. En vanlig måte er å bruke `JSONSerialization`, men det finnes også populære biblioteker som SwiftyJSON som gjør det enklere å hente ut verdier fra JSON-strenger. Det er også viktig å huske på at JSON er et hierarkisk format, og at man bør strukturere dataene sine på en måte som gjør det enkelt å hente ut ønskede verdier.

## Se også

- [Apple documentation for JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [SwiftyJSON on GitHub](https://github.com/SwiftyJSON/SwiftyJSON)
- [How to work with JSON in Swift](https://www.raywenderlich.com/3627073-json-tutorial-for-ios-getting-started)