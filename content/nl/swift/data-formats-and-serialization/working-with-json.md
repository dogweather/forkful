---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:56.399058-07:00
description: 'Hoe te: Swift maakt JSON-verwerking eenvoudig met het `Codable` protocol.
  Hier is hoe je JSON decodeert naar een Swift-object.'
lastmod: '2024-03-13T22:44:51.179408-06:00'
model: gpt-4-0125-preview
summary: Swift maakt JSON-verwerking eenvoudig met het `Codable` protocol.
title: Werken met JSON
weight: 38
---

## Hoe te:
Swift maakt JSON-verwerking eenvoudig met het `Codable` protocol. Hier is hoe je JSON decodeert naar een Swift-object:

```Swift
import Foundation

// Definieer een model dat voldoet aan Codable
struct Gebruiker: Codable {
    var naam: String
    var leeftijd: Int
}

// JSON-tekenreeks
let jsonString = """
{
    "naam": "John Doe",
    "leeftijd": 30
}
"""

// Converteer JSON-tekenreeks naar Data
if let jsonData = jsonString.data(using: .utf8) {
    // Decodeer JSON-gegevens naar Gebruiker-object
    do {
        let gebruiker = try JSONDecoder().decode(Gebruiker.self, from: jsonData)
        print("Naam: \(gebruiker.naam), Leeftijd: \(gebruiker.leeftijd)")
    } catch {
        print("Fout bij het decoderen van JSON: \(error)")
    }
}
```

Voorbeeld uitvoer:
```
Naam: John Doe, Leeftijd: 30
```

## Diepere Duik
JSON (JavaScript Object Notation) is sinds het begin van de jaren 2000 op grote schaal aangenomen, nadat Douglas Crockford het gespecificeerd had. Het verving XML voor veel gebruikssituaties vanwege zijn eenvoudigere syntaxis en betere prestaties. Hoewel Swift's `Codable` de voorkeur heeft voor JSON, bestaan er alternatieven zoals `JSONSerialization` voor wanneer je te maken hebt met niet-Codable-conforme types. Intern maakt `Codable` de lagere-niveau parsing abstract en maakt serialisatie/deserilisatie naadloos.

## Zie Ook
- Verken meer over JSON en Swift op de officiële Swift-blog: [Swift.org](https://swift.org/blog/)
- Bekijk de documentatie van `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Voor complexe JSON-structuren, overweeg bibliotheken van derden zoals SwiftyJSON beschikbaar op [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
