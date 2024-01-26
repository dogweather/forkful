---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:57:04.755367-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Kommandozeilenargumenten ermöglicht es deinen Swift-Programmen, beim Start Input zu erhalten – praktisch für flexible Tools und Automatisierung. Programmierer nutzen das, um ihre Anwendungen an verschiedene Szenarien anzupassen, ohne den Code zu ändern.

## So geht's:
Swift macht das Einlesen von Kommandozeilenargumenten einfach. `CommandLine.arguments` enthält alle Argumente als `[String]`, direkt loslegen:

```swift
// main.swift
for arg in CommandLine.arguments {
    print(arg)
}
```

Wenn du das Programm mit `swift main.swift eins zwei drei` ausführst, bekommst du:

```
main.swift
eins
zwei
drei
```

## Tiefgang:
Historisch gesehen kommen Kommandozeilenargumente aus den Zeiten vor grafischen Oberflächen. Nutzer interagierten textbasiert mit dem Betriebssystem.

Alternativen zu `CommandLine.arguments` inkludieren Umgebungsvariablen (`ProcessInfo.processInfo.environment`) oder spezielle Parsing-Bibliotheken, die mehr Komplexität erlauben, etwa [Swift Argument Parser](https://github.com/apple/swift-argument-parser).

Bei der Implementierung solltest du berücksichtigen, dass `CommandLine.arguments` das erste Argument, den Pfad zur ausführbaren Datei, immer enthält. Nicht-Kommandozeilenprogramme sollten das nicht nutzen – es gibt bessere Wege, um mit einem Benutzer zu interagieren.

## Siehe auch:
- Apple's Dokumentation zu [ProcessInfo](https://developer.apple.com/documentation/foundation/processinfo) und [CommandLine](https://developer.apple.com/documentation/swift/commandline).
- Der offizielle Swift Blog Beitrag über den [Swift Argument Parser](https://swift.org/blog/argument-parser/).
- Eine Anleitung zum Parsing von Kommandozeilenargumenten mit dem Swift Argument Parser: [Ray Wenderlich Tutorial](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial).
