---
title:                "Arbeit mit yaml"
html_title:           "Swift: Arbeit mit yaml"
simple_title:         "Arbeit mit yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Arbeit mit YAML ist ein Weg für Programmierer, Daten in einem menschenlesbaren Format zu speichern und abzurufen. Es ist nützlich für Konfigurationsdateien und Daten serialisierung. Programmierer nutzen YAML, um ihre Daten in einer strukturierten und einfach zu lesenden Art und Weise zu organisieren.

## So geht's:
```Swift
// Beispiel Code zum Lesen von Daten aus einer YAML Datei und Zugriff auf spezifische Schlüssel und Werte
do {
    // Öffnen der Datei und Parsen der Daten in ein Dictionary
    let file = try File(path: "data.yaml")
    let yamlData = try Yaml.load(file.readAsString())

    // Zugriff auf den Wert des Schlüssels "name"
    let name = yamlData["name"].string!

    // Zugriff auf alle Werte unter dem Schlüssel "dogs"
    let dogs = yamlData["dogs"]

    // Ausgabe des Namens und der Anzahl der Hunde
    print("Mein Name ist \(name) und ich habe \(dogs.count!) Hunde.")
} catch {
    // Fehlerbehandlung
    print("Ein Fehler ist aufgetreten.")
}
```

## Tiefer Einblick:
YAML wurde ursprünglich für die Programmiersprache Perl entwickelt und steht für "YAML Ain't Markup Language". Es ist eine Alternative zu anderen Datenformaten wie XML oder JSON und wird häufig in Webanwendungen verwendet. In Swift kann YAML mithilfe von Bibliotheken wie "Yams" und "SwiftYAML" verarbeitet werden.

## Siehe auch:
- [YAML-Spezifikation](https://yaml.org/spec/)
- [Yams Bibliothek für Swift](https://github.com/jpsim/Yams)
- [SwiftYAML Bibliothek](https://github.com/behrang/YamlSwift)