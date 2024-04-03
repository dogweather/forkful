---
date: 2024-01-19
description: "JSON (JavaScript Object Notation) ist ein Datenformat zum Austausch\
  \ von Datenobjekten. Programmierer nutzen es, weil es leichtgewichtig, textbasiert,\u2026"
lastmod: '2024-03-13T22:44:54.246077-06:00'
model: unknown
summary: JSON (JavaScript Object Notation) ist ein Datenformat zum Austausch von Datenobjekten.
title: Arbeiten mit JSON
weight: 38
---

## Was & Warum?
JSON (JavaScript Object Notation) ist ein Datenformat zum Austausch von Datenobjekten. Programmierer nutzen es, weil es leichtgewichtig, textbasiert, lesbar und einfach zu verarbeiten ist.

## How to:
Swift macht das Arbeiten mit JSON mit der `Codable`-Schnittstelle einfach. Hier ist ein Grundbeispiel: 

```Swift
import Foundation

// Definiere ein Codable Model
struct User: Codable {
    var name: String
    var age: Int
}

// JSON-String
let jsonString = "{\"name\": \"Anna\", \"age\": 28}"

// JSON in ein User-Objekt umwandeln
if let jsonData = jsonString.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print(user) // Output: User(name: "Anna", age: 28)
    } catch {
        print("Fehler beim Parsen: \(error)")
    }
}

// Ein User-Objekt in JSON umwandeln
let user = User(name: "Max", age: 35)
do {
    let jsonData = try JSONEncoder().encode(user)
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString) // Output: {"name":"Max","age":35}
    }
} catch {
    print("Fehler beim Konvertieren: \(error)")
}
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre als Alternative zu XML beliebt. Es ist einfacher zu lesen und zu schreiben. Alternativen wie YAML oder BSON existieren, aber JSON dominiert wegen seiner Einfachheit und Allgegenwart. Swifts `Codable`-Schnittstelle, eingeführt in Swift 4, hat das Codieren und Decodieren von JSON erheblich vereinfacht, da es Auto-Synthese von `CodingKeys` ermöglicht und viel Boilerplate-Code unnötig macht.

## See Also
- [Swift.org: Encoding and Decoding Custom Types](https://swift.org/documentation/#encoding-and-decoding-custom-types)
- [JSON on Wikipedia](https://de.wikipedia.org/wiki/JavaScript_Object_Notation)
- [Apple Documentation on JSONEncoder](https://developer.apple.com/documentation/foundation/jsonencoder)
- [Apple Documentation on JSONDecoder](https://developer.apple.com/documentation/foundation/jsondecoder)
