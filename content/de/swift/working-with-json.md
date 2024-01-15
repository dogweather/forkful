---
title:                "Arbeiten mit json"
html_title:           "Swift: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Wenn du als Entwickler(in) mit Webanwendungen oder mobilen Apps arbeitest, wirst du wahrscheinlich auf Dateiformate wie JSON stoßen. JSON, oder Javascript Object Notation, ist ein verbreitetes Format zum Austausch von Daten zwischen verschiedenen Plattformen. In diesem Artikel erfährst du, wie du JSON in deine Swift Anwendungen integrieren kannst.

## Wie geht's?

Wenn du JSON in deine Swift Anwendung einbinden möchtest, benötigst du zuerst eine JSON Datei, die die gewünschten Daten enthält. Diese kannst du entweder von einer externen Quelle herunterladen oder selbst erstellen.

Als nächstes musst du die Foundation Framework importieren:

```Swift
import Foundation
```

Dann kannst du die JSON Datei in dein Projekt einfügen und als NSData Objekt initialisieren:

```Swift
if let path = Bundle.main.path(forResource: "data", ofType: "json") {
    if let data = try? NSData(contentsOfFile: path, options: .mappedIfSafe) {
        //weiterer Code zum Manipulieren der Daten
    }
}
```

Nun kannst du mit der Foundation Klasse `JSONSerialization` die Daten auslesen und in ein Dictionary oder Array umwandeln:

```Swift
let json = try JSONSerialization.jsonObject(with: data as Data, options: .allowFragments) as! [String: Any]
```

Jetzt hast du die Daten als Swift Objekte zur Verfügung und kannst damit arbeiten. Zum Beispiel könntest du das Dictionary durchlaufen und die Werte ausgeben:

```Swift
for (key, value) in json {
    print("\(key): \(value)")
}
```

## Tiefer Graben

Um noch besser mit JSON in Swift arbeiten zu können, gibt es einige nützliche Konzepte und Bibliotheken, die es dir ermöglichen, komplexere Datenstrukturen zu verarbeiten.

Ein großer Vorteil von Swift ist die Möglichkeit, benutzerdefinierte Objekte zu erstellen. Du kannst beispielsweise eine Struktur erstellen, die die Daten aus deiner JSON Datei beinhaltet und diese dann mit der `Codable` Protokoll implementiert:

```Swift
struct Person: Codable {
    var name: String
    var age: Int
}
```

Dann kannst du die Daten auslesen und direkt in dein benutzerdefiniertes Objekt umwandeln:

```Swift
if let data = try? NSData(contentsOfFile: path, options: .mappedIfSafe) {
    let decoder = JSONDecoder()
    let person = try decoder.decode(Person.self, from: data as Data)
    print(person.name) //Ausgabe: Max Mustermann
}
```

Zusätzlich gibt es auch Bibliotheken wie SwiftyJSON oder ObjectMapper, die das Parsen von JSON noch einfacher und flexibler machen.

## Siehe Auch

- [Apple Developer Dokumentation zu JSON](https://developer.apple.com/documentation/foundation/json)
- [SwiftyJSON Github Repository](https://github.com/SwiftyJSON/SwiftyJSON)
- [Object Mapper Github Repository](https://github.com/Hearst-DD/ObjectMapper)