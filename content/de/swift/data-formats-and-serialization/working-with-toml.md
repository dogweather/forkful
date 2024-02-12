---
title:                "Arbeiten mit TOML"
aliases: - /de/swift/working-with-toml.md
date:                  2024-01-26T04:26:23.857387-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML (Tom's Offensichtliche, Minimale Sprache) ist ein Daten-Serialisierungsformat, das dank seiner klaren Semantik leicht zu lesen ist. Programmierer verwenden TOML für Konfigurationsdateien, bei denen die Lesbarkeit durch Menschen und einfaches Parsen durch Maschinen Schlüsselrollen spielen.

## Wie geht das:
Zunächst benötigen Sie einen TOML-Parser. Swift hat keinen eingebauten, also verwenden wir `TOMLDecoder`. Installieren Sie ihn über den Swift Package Manager und dann können Sie TOML mühelos serialisieren und deserialisieren.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Beispiel"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Eigentümer
}

struct Eigentümer: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Titel: \(config.title), Besitzer: \(config.owner.name), Geb.Datum: \(config.owner.dob)")
    } catch {
        print("Fehler beim Parsen von TOML: \(error)")
    }
}
```

Dieser Code gibt aus:
```
Titel: TOML Beispiel, Besitzer: Tom Preston-Werner, Geb.Datum: 1979-05-27 07:32:00 +0000
```

## Tiefergehend
TOML wurde von Tom Preston-Werner, dem Mitbegründer von GitHub, als eine benutzerfreundlichere Alternative zu Formaten wie JSON oder YAML entwickelt. Es zielt auf Klarheit ab, die Chancen einer Fehlinterpretation durch einen Menschen oder eine Maschine zu reduzieren. Was Alternativen angeht, so sind YAML und JSON die üblichen Verdächtigen, wobei YAML eher auf menschliche Lesbarkeit ausgerichtet ist und JSON als die einfachere maschinenfreundliche Option gilt. Beim Arbeiten mit TOML in Swift haben wir keinen nativen Parser. Jedoch erleichtern Drittanbieterbibliotheken wie `TOMLDecoder` die einfache Konvertierung zwischen TOML-Strings und Swift-Typen, insbesondere über die in Swift 4 eingeführten `Codable`-Protokolle, die die Serialisierung vereinfacht haben.

## Siehe auch
- Der TOML-Standard: https://toml.io
- GitHub für `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Swift-Dokumentation zu `Codable`: https://developer.apple.com/documentation/swift/codable
- Vergleich von Daten-Serialisierungsformaten: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
