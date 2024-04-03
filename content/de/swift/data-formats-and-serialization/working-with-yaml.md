---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:51.370456-07:00
description: "Wie geht das: Swift bietet keine integrierte Unterst\xFCtzung f\xFC\
  r YAML-Parsing und -Serialisierung, was die Verwendung von Drittanbieter-Bibliotheken\u2026"
lastmod: '2024-03-13T22:44:54.245118-06:00'
model: gpt-4-0125-preview
summary: "Swift bietet keine integrierte Unterst\xFCtzung f\xFCr YAML-Parsing und\
  \ -Serialisierung, was die Verwendung von Drittanbieter-Bibliotheken erforderlich\
  \ macht."
title: Arbeiten mit YAML
weight: 41
---

## Wie geht das:
Swift bietet keine integrierte Unterstützung für YAML-Parsing und -Serialisierung, was die Verwendung von Drittanbieter-Bibliotheken erforderlich macht. Eine beliebte Wahl ist `Yams`, eine Bibliothek zum Arbeiten mit YAML in Swift.

Zuerst müssen Sie `Yams` zu Ihrem Projekt hinzufügen. Wenn Sie den Swift Package Manager verwenden, können Sie es als Abhängigkeit in Ihrer `Package.swift`-Datei hinzufügen:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### YAML in Swift parsen
Nehmen wir an, Sie haben die folgende YAML-Konfiguration für eine einfache App:

```yaml
name: MyApp
version: 1.0
umgebung: development
features:
  - login
  - benachrichtigungen
```

So können Sie diesen YAML-String in Swift mit `Yams` parsen:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
umgebung: development
features:
  - login
  - benachrichtigungen
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Beispielzugriff auf die geparsten Daten
        if let name = data["name"] as? String {
            print("App-Name: \(name)")
        }
    }
} catch {
    print("Fehler beim Parsen von YAML: \(error)")
}
```

Beispielausgabe:

```
["name": MyApp, "version": 1.0, "umgebung": "development", "features": ["login", "benachrichtigungen"]]
App-Name: MyApp
```

### Swift-Objekte in YAML serialisieren
Ein Swift-Objekt zurück in einen YAML-String zu konvertieren, ist ebenfalls unkompliziert mit `Yams`. Nehmen wir an, Sie haben dieselbe Datenstruktur, die serialisiert werden muss:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "umgebung": "development",
    "features": ["login", "benachrichtigungen"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Fehler bei der Serialisierung zu YAML: \(error)")
}
```

Dies wird einen YAML-formatierten String produzieren:

```yaml
umgebung: development
features:
  - login
  - benachrichtigungen
name: MyApp
version: 1.0
```

Diese Beispiele demonstrieren grundlegende Operationen für die Arbeit mit YAML in Swift-Anwendungen. Denken Sie daran, während YAML in Bezug auf menschliche Lesbarkeit und Benutzerfreundlichkeit hervorragend ist, sollten Sie immer die spezifischen Bedürfnisse Ihrer Anwendung berücksichtigen, insbesondere in Bezug auf Leistung und Komplexität, wenn Sie Ihr Daten-Serialisierungsformat wählen.
