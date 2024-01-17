---
title:                "Arbeiten mit Json"
html_title:           "Swift: Arbeiten mit Json"
simple_title:         "Arbeiten mit Json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn du als Programmierer auf JSON (JavaScript Object Notation) stößt, bedeutet das, dass du dich mit strukturierten Daten auseinandersetzt. Das kann nützlich sein, wenn du Daten zwischen verschiedenen Programmiersprachen oder Plattformen austauschen möchtest. JSON ist eine sehr beliebte Wahl bei der Datenformatierung, da es leicht zu lesen und zu verstehen ist. 

## Wie geht's?
Das Arbeiten mit JSON in Swift ist einfach und unkompliziert. Beginne damit, eine Instanz von ```JSONDecoder``` zu erstellen und weise sie einer Variablen zu. Dann verwende die Methode ```decode()```, um JSON-Daten zu dekodieren. Zum Beispiel:

```
let jsonDecoder = JSONDecoder()
let jsonData = """
    {
        "name": "Max Mustermann",
        "age": 25,
        "hobbies": ["coding", "gaming", "hiking"]
    }
""".data(using: .utf8)
let person = try jsonDecoder.decode(Person.self, from: jsonData)
print(person.name) // Ausgabe: Max Mustermann
print(person.age) // Ausgabe: 25
print(person.hobbies) // Ausgabe: ["coding", "gaming", "hiking"]
```

Du kannst auch benutzerdefinierte Objekte erstellen und sie in JSON umwandeln. Verwende dazu die Methode ```encode()``` und weise das Ergebnis einer Variablen zu. Zum Beispiel:

```
struct Person: Codable {
    let name: String
    let age: Int
    let hobbies: [String]
}

let person = Person(name: "Max Mustermann", age: 25, hobbies: ["coding", "gaming", "hiking"])
let jsonEncoder = JSONEncoder()
jsonEncoder.outputFormatting = .prettyPrinted
let jsonData = try jsonEncoder.encode(person)
print(String(data: jsonData, encoding: .utf8)!) // Ausgabe: {"name" : "Max Mustermann",
                                               //          "age" : 25,
                                               //          "hobbies" : ["coding", "gaming", "hiking"]
                                               //         }
```

## Tiefgehende Einblicke
JSON wurde als Alternative zu XML entwickelt, da es weniger komplex und leichter zu verarbeiten ist. Es wurde als Teil von JavaScript eingeführt, ist aber mittlerweile eine unabhängige Datenformatierung. JSON wird häufig verwendet, um Daten zwischen Webanwendungen und Servern auszutauschen. Es gibt auch alternative Datenformatierungen wie XML, CSV und YAML, aber JSON ist aufgrund seiner Einfachheit und weit verbreiteten Unterstützung die beliebteste Wahl.

## Siehe auch
- [Apple Dokumentation zu JSON](https://developer.apple.com/documentation/foundation/archives_and_serialization/using_json_with_custom_types)
- [JSON Tutorial für Swift Einsteiger](https://www.ioscreator.com/tutorials/json-swift)
- [Liste von alternativen Datenformatierungen](https://developers.squarespace.com/what-is-json/)