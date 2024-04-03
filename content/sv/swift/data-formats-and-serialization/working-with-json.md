---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:14.618570-07:00
description: "Hur man g\xF6r: Swift g\xF6r JSON-tolkning enkelt med `Codable`-protokollet.\
  \ S\xE5 h\xE4r avkodar du JSON till ett Swift-objekt."
lastmod: '2024-03-13T22:44:38.272874-06:00'
model: gpt-4-0125-preview
summary: "Swift g\xF6r JSON-tolkning enkelt med `Codable`-protokollet."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:
Swift gör JSON-tolkning enkelt med `Codable`-protokollet. Så här avkodar du JSON till ett Swift-objekt:

```Swift
import Foundation

// Definiera en modell som följer Codable
struct Användare: Codable {
    var namn: String
    var ålder: Int
}

// JSON-sträng
let jsonString = """
{
    "namn": "John Doe",
    "ålder": 30
}
"""

// Konvertera JSON-sträng till Data
if let jsonData = jsonString.data(using: .utf8) {
    // Avkoda JSON-data till Användarobjekt
    do {
        let användare = try JSONDecoder().decode(Användare.self, from: jsonData)
        print("Namn: \(användare.namn), Ålder: \(användare.ålder)")
    } catch {
        print("Fel vid avkodning av JSON: \(error)")
    }
}
```

Exempel på utskrift:
```
Namn: John Doe, Ålder: 30
```

## Fördjupning
JSON (JavaScript Object Notation) har varit allmänt antaget sedan början av 2000-talet, efter att Douglas Crockford specificerat det. Det har ersatt XML för många användningsfall på grund av sin enklare syntax och bättre prestanda. Medan Swifts `Codable` är standardvalet för JSON, finns alternativ som `JSONSerialization` för när man hanterar typer som inte är kompatibla med Codable. Bakom kulisserna abstraherar `Codable` bort den lägre nivåns tolkning och gör serialisering/avserialisering sömlös.

## Se även
- Utforska mer om JSON och Swift i den officiella Swift-bloggen: [Swift.org](https://swift.org/blog/)
- Kolla in `Codable`-dokumentationen: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- För komplexa JSON-strukturer, överväg tredjepartbibliotek såsom SwiftyJSON tillgängligt på [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
