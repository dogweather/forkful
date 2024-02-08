---
title:                "Arbeta med JSON"
aliases:
- sv/swift/working-with-json.md
date:                  2024-02-03T19:24:14.618570-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON i Swift innebär att hantera ett lättviktigt dataformat för datautbyte. Programmerare använder JSON för att överföra data mellan en server och en webbapplikation eftersom det är läsbart och lätt att tolka för både människor och maskiner.

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
