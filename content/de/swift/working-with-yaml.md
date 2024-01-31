---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML, kurz für "YAML Ain't Markup Language", ist ein Datenserialisierungsformat, das für seine Klarheit und Menschenlesbarkeit bekannt ist. Programmierer verwenden es häufig für Konfigurationsdateien oder zum Datenaustausch, weil es einfacher zu lesen und zu schreiben ist als XML oder JSON.

## How to:
Swift hat keine eingebaute YAML-Unterstützung, aber mit der `Yams`-Bibliothek kannst du YAML Daten leicht verarbeiten. Zuerst installierst du Yams über Swift Package Manager:

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "DeinProjekt",
    dependencies: [
        .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0"),
    ],
    targets: [
        .target(name: "DeinProjekt", dependencies: ["Yams"]),
    ]
)
```

Dann importierst du Yams und parsest YAML-String in Swift:

```swift
import Yams

let yamlString = """
- name: Max Mustermann
  age: 30
- name: Erika Mustermann
  age: 28
"""

do {
    let daten = try Yams.load(yaml: yamlString) as? [[String: Any]]
    if let daten = daten {
        for person in daten {
            print("\(person["name"] ?? "") ist \(person["age"] ?? "") Jahre alt.")
        }
    }
} catch {
    print("Parsing Fehler: \(error)")
}
```

Ausgabe:

```
Max Mustermann ist 30 Jahre alt.
Erika Mustermann ist 28 Jahre alt.
```

## Deep Dive:
YAML entstand Anfang der 2000er Jahre und sollte eine einfachere Alternative zu XML sein. Im Vergleich zu JSON ist YAML besser für Konfigurationen geeignet, da es kommentierbar ist und weniger Klammern verwendet. Performancewise ist JSON schneller zu parsen, daher bevorzugt man YAML in menschenlesbaren Szenarien und JSON, wenn Geschwindigkeit entscheidend ist.

Für Swift gibt es, neben `Yams`, Alternativen wie `Swift-YAML` oder direktes Parsen zu JSON mit anschließender Umwandlung. `Yams` ist jedoch die beliebteste Option, nicht zuletzt wegen seiner guten Dokumentation und Community-Unterstützung.

## See Also:
Weitere Ressourcen findest du hier:

- Yams GitHub Repository: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- YAML Offizielle Webseite: [https://yaml.org](https://yaml.org)
- Swift Package Manager Dokumentation: [https://swift.org/package-manager/](https://swift.org/package-manager/)
