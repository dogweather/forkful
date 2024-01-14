---
title:                "Swift: Arbeiten mit YAML."
simple_title:         "Arbeiten mit YAML."
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

YAML ist eine einfache und übersichtliche Möglichkeit, um Daten zu strukturieren und zu speichern. Als Programmierer kann man davon profitieren, da es die Lesbarkeit des Codes verbessert und die Wartbarkeit erleichtert.

## Wie man YAML verwendet

Um YAML in Swift zu verwenden, muss zunächst die YAML-Bibliothek eingebunden werden. Das folgende Beispiel zeigt, wie man dies in Xcode tun kann:
```Swift
// Importiere YAML-Bibliothek
import Yams 

// Definiere ein Dictionary mit Daten im YAML-Format
let yamlString = """
name: John Smith
age: 30
hobbies:
- coding
- hiking
- reading
"""

// Parse das YAML-String zu Swift Dictionary
let dictionary = try Yams.load(yaml: yamlString) as! [String:Any]
print(dictionary)

// Output: ["name": "John Smith", "age": 30, "hobbies": ["coding", "hiking", "reading"]]
```

Man kann auch YAML-Dateien direkt laden und parsen, anstatt sie als String zu definieren:

```Swift
// Lade YAML-Datei in Swift Dictionary
let dictionary = try Yams.load(yaml: "data.yml") as! [String:Any]
```

## Tiefere Einblicke in YAML

YAML unterstützt verschiedene Datentypen wie Strings, Zahlen, Arrays und Dictionaries. Es ist auch möglich, benutzerdefinierte Objekte in YAML zu definieren und zu laden.

Ein weiterer Vorteil von YAML ist die Möglichkeit, Kommentare hinzuzufügen, um den Code lesbarer zu machen. Kommentare werden mit dem Symbol `#` eingeleitet.

Es ist auch möglich, verschachtelte Strukturen in YAML zu erstellen, indem man die Einrückung verwendet. Zum Beispiel kann man eine Liste von Personen mit ihren Hobbys definieren:

```YAML
persons:
  - name: John Smith
    age: 30
    hobbies:
      - coding
      - hiking
      - reading
  - name: Maria Schmidt
    age: 25
    hobbies:
      - painting
```

## Siehe auch

- [Yams GitHub Repository](https://github.com/jpsim/Yams)
- [YAML Tutorial](https://www.tutorialspoint.com/yaml/index.htm)
- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)