---
title:                "Swift: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

JSON ist eine häufig verwendete Formatierung für den Austausch von Daten in der Softwareentwicklung. Wenn du als Entwickler*in lernen möchtest, wie du mit Daten in deinen Swift-Projekten umgehen kannst, ist es wichtig, sich mit JSON vertraut zu machen. Mit Swift ist es relativ einfach, JSON-Daten zu lesen und zu schreiben, wodurch dein Code flexibler und erweiterbarer wird.

## Anleitung

Um mit JSON in Swift zu arbeiten, gibt es einige wichtige Schritte zu beachten. Zunächst musst du sicherstellen, dass du die Foundation-Bibliothek in dein Projekt importierst. Dies ermöglicht es dir, die JSON-Funktionalität von Swift zu nutzen. Dann kannst du mit dem Decoden und Encoden von Daten beginnen.

Hier ist ein Beispiel, wie du JSON-Daten mit Swift decodieren kannst:

```Swift
// Beispiel JSON-Daten
let json = """
{
    "name": "Max Mustermann",
    "age": 28,
    "hobbies": ["Skifahren", "Kochen", "Lesen"]
}
""".data(using: .utf8)!

// Dekodierung in ein Swift-Objekt
struct Person: Codable {
    var name: String
    var age: Int
    var hobbies: [String]
}

do {
    let person = try JSONDecoder().decode(Person.self, from: json)
    print(person.name) //Ausgabe: Max Mustermann
    print(person.hobbies) //Ausgabe: ["Skifahren", "Kochen", "Lesen"]
} catch {
    print(error)
}
```

Und so kannst du JSON-Daten mit Swift encodieren:

```Swift
// Swift-Objekt, das encoded werden soll
let person = Person(name: "Anna Schmidt", age: 35, hobbies: ["Reisen", "Fotografieren"])

do {
    let jsonData = try JSONEncoder().encode(person)
    let jsonString = String(data: jsonData, encoding: .utf8)!
    print(jsonString) //Ausgabe: {"name":"Anna Schmidt","age":35,"hobbies":["Reisen","Fotografieren"]}
} catch {
    print(error)
}
```

## Eintauchen in die Tiefe

Es gibt verschiedene Techniken, um mit JSON-Daten in Swift zu arbeiten, je nachdem wie komplex die Datenstruktur ist. Du kannst zum Beispiel die Codable-Protokolle nutzen, wenn die JSON-Struktur deinen Swift-Modellen entspricht. Aber es gibt auch Situationen, in denen du manuell mit JSON-Daten arbeiten musst, z.B. wenn die Datenstruktur dynamisch ist oder spezielle Anforderungen hat. In solchen Fällen kannst du APIs wie SwiftyJSON oder ObjectMapper verwenden, um die Daten zu parsen und in Swift-Objekte umzuwandeln.

Es ist auch wichtig zu wissen, wie du mit optionalem Data-Typen in JSON umgehen kannst, da es immer Fälle gibt, in denen bestimmte Daten fehlen oder die Datenstruktur unvollständig ist. Mit Swift-Optionals kannst du sicherstellen, dass dein Code nicht abstürzt, wenn Daten fehlen.

## Siehe auch

- [Apple Dokumentation zu JSON in Swift](https://developer.apple.com/documentation/foundation/jsonobject)
- [SwiftyJSON - Einfache JSON-Manipulation in Swift](https://github.com/SwiftyJSON/SwiftyJSON)
- [ObjectMapper - JSON-Mapping-Framework für Swift](https://github.com/tristanhimmelman/ObjectMapper)