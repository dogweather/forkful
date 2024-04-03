---
date: 2024-01-26 00:57:20.604419-07:00
description: "Fehlerbehandlung in Swift bedeutet, auf Probleme zu reagieren, die auftreten,\
  \ wenn Ihr Code ausgef\xFChrt wird. Wir tun dies, um das Chaos zu kontrollieren\
  \ \u2013\u2026"
lastmod: '2024-03-13T22:44:54.232671-06:00'
model: gpt-4-1106-preview
summary: "Fehlerbehandlung in Swift bedeutet, auf Probleme zu reagieren, die auftreten,\
  \ wenn Ihr Code ausgef\xFChrt wird."
title: Fehlerbehandlung
weight: 16
---

## Was & Warum?
Fehlerbehandlung in Swift bedeutet, auf Probleme zu reagieren, die auftreten, wenn Ihr Code ausgeführt wird. Wir tun dies, um das Chaos zu kontrollieren – um zu verhindern, dass Apps abstürzen und um dem Benutzer eine reibungslose Erfahrung zu bieten.

## Wie geht das:
Swift verwendet Fehlerbehandlung mit `do`, `try` und `catch` Blöcken. Schauen wir uns das an:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Stellen Sie sich vor, wir haben hier etwas Logik, um zu prüfen, ob eine Datei existiert und ob wir die Berechtigung zum Lesen haben
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Dateiinhalt geht hier"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Hoppla! Datei nicht gefunden.")
} catch FileError.noPermission {
    print("Ah! Keine Berechtigung, um die Datei zu lesen.")
} catch {
    print("Ein unbekannter Fehler ist aufgetreten.")
}

```

Beispielausgabe:

```
Hoppla! Datei nicht gefunden.
```

## Vertiefung
Die Fehlerbehandlung war nicht immer so elegant wie jetzt. In Objective-C hatte man es mit Zeigern auf NSError-Objekte zu tun, was recht umständlich war. Jetzt haben wir ein eleganteres System mit Swift Aufzählungen (enums) und dem `Error`-Protokoll.

Swifts `throw` lässt uns signalisieren, dass etwas schiefgegangen ist. `do`-Blöcke fungieren wie fehlerbewusste Bereiche, `try` leitet die riskanten Vorgänge ein, und `catch` kümmert sich um die Dinge, falls sie schiefgehen.

Optionals sind eine Alternative für Situationen, die nicht ganz den "Fehler"-Status erreichen, aber dennoch "kein Ergebnis" haben könnten. Sie sind ein bisschen wie Schrödingers Variablen - sie haben einen Wert oder sie haben keinen.

Für echte Tiefe schauen Sie sich `Result`-Typen an, die schicke Hybride zwischen normaler Rückgabe und Fehlermustern sind.

## Siehe auch
- Offizieller Swift Fehlerbehandlung Leitfaden: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Beste Praktiken für die Swift Fehlerbehandlung: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Fortgeschrittene Fehlerbehandlung in Swift: [Medium Artikel](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
