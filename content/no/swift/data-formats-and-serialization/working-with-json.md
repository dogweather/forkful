---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:07.266765-07:00
description: "Hvordan: Swift gj\xF8r tolking av JSON enkelt med `Codable`-protokollen.\
  \ Her er hvordan du dekoder JSON til et Swift-objekt."
lastmod: '2024-03-13T22:44:41.163204-06:00'
model: gpt-4-0125-preview
summary: "Swift gj\xF8r tolking av JSON enkelt med `Codable`-protokollen."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Swift gjør tolking av JSON enkelt med `Codable`-protokollen. Her er hvordan du dekoder JSON til et Swift-objekt:

```Swift
import Foundation

// Definer en modell som overholder Codable
struct User: Codable {
    var name: String
    var age: Int
}

// JSON-streng
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Konverter JSON-streng til Data
if let jsonData = jsonString.data(using: .utf8) {
    // Dekod JSON-data til User-objektet
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Navn: \(user.name), Alder: \(user.age)")
    } catch {
        print("Feil ved dekoding av JSON: \(error)")
    }
}
```

Eksempel på resultat:
```
Navn: John Doe, Alder: 30
```

## Dypdykk
JSON (JavaScript Object Notation) har vært mye brukt siden tidlig på 2000-tallet, etter at Douglas Crockford spesifiserte det. Det erstattet XML i mange bruksområder på grunn av sin enklere syntaks og bedre ytelse. Mens Swifts `Codable` er gå-til-løsningen for JSON, eksisterer alternativer som `JSONSerialization` for når du håndterer typer som ikke er kompatible med Codable. Bak kulissene abstraherer `Codable` bort lavnivåtolkingen og gjør serialisering/deserialisering sømløs.

## Se Også
- Utforsk mer om JSON og Swift på det offisielle Swift-blogget: [Swift.org](https://swift.org/blog/)
- Sjekk ut `Codable`-dokumentasjonen: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- For komplekse JSON-strukturer, vurder tredjepartsbiblioteker som SwiftyJSON tilgjengelig på [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
