---
title:                "Arbeiten mit yaml"
html_title:           "Swift: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum

YAML (Yet Another Markup Language) ist eine einfach zu lesende und schreibende Auszeichnungssprache, die häufig für die Konfiguration von Software verwendet wird. Wenn du also in der Softwareentwicklung tätig bist, ist es wichtig, YAML zu verstehen und zu beherrschen, um effizienter arbeiten zu können. 

# Wie geht's?

Um YAML in Swift zu nutzen, gibt es verschiedene Möglichkeiten. Hier sind drei Beispiele, wie du mit YAML in Swift arbeiten kannst:

### Beispiel 1: YAML-Datei lesen

Um eine YAML-Datei in Swift zu lesen, kannst du die `YamlSwift` Bibliothek verwenden. Zunächst musst du die Bibliothek in deinem Projekt importieren:

```Swift
import YamlSwift
```

Dann kannst du die YAML-Datei mit dem folgenden Code lesen:

```Swift
let yaml = try Yaml.load("config.yaml")
```

Das Ergebnis ist ein Objekt vom Typ `Yaml`, das du dann weiterverarbeiten kannst.

### Beispiel 2: YAML-Daten speichern

Wenn du YAML-Daten erstellen und speichern möchtest, kannst du dies auf einfache Weise mit der `YamlSwift` Bibliothek tun. Hier ist ein Beispiel, wie du ein einfaches `Dictionary` in YAML-Format konvertieren und in eine Datei schreiben kannst:

```Swift
let data = ["name": "Max", "age": 26]
let yaml = Yaml.dictionary(data)
try yaml.save("profile.yaml")
```

Damit hast du eine Datei mit folgendem Inhalt erstellt:

```yaml
name: Max
age: 26
```

### Beispiel 3: YAML in Swift struct umwandeln

Eine andere Möglichkeit, mit YAML in Swift zu arbeiten, ist die Verwendung von `Codable`. Hiermit kannst du eine YAML-Datei direkt in eine Struct umwandeln. Dazu musst du zunächst die `CodableYaml` Bibliothek importieren:

```Swift
import CodableYaml
```

Dann kannst du deine Struct mit `Codable` annotieren und die `YamlDecoder`-Klasse verwenden, um die YAML-Datei zu dekodieren:

```Swift
struct Person: Codable {
    var name: String
    var age: Int
}

let data = try! Data(contentsOf: URL(fileURLWithPath: "profile.yaml"))
let decoder = YamlDecoder()
let person = try decoder.decode(Person.self, from: data)
```

Jetzt kannst du auf die Werte der YAML-Datei wie folgt zugreifen:

```Swift
print(person.name) // Output: Max
print(person.age) // Output: 26
```

# Tiefergehende Informationen

Jetzt weißt du, wie du mit YAML in Swift arbeiten kannst. Aber vielleicht interessieren dich noch einige weitere Informationen zum Thema. Hier sind einige Ressourcen, die dir helfen können, tiefer in das Thema einzusteigen:

- [Offizielle YAML-Website](https://yaml.org/)
- [Swift Package für die Verarbeitung von YAML-Dateien](https://github.com/behrang/YamlSwift)
- [CodableYaml Package für Codable-Unterstützung](https://github.com/jpsim/CodableYAML)

# Siehe auch

- [Einführung in YAML für Swift-Entwickler](https://medium.com/better-programming/yaml-for-swift-developers-7d16c8bcfaef)
- [YAML vs. JSON: Was ist der Unterschied?](https://docs.fileformat.com/yaml-vs-json/)
- [Tutorial: Wie man mit Structs in Swift arbeitet](https://www.hackingwithswift.com/sixty/5/1/structs)