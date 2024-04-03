---
date: 2024-01-20 17:57:04.755367-07:00
description: "Das Lesen von Kommandozeilenargumenten erm\xF6glicht es deinen Swift-Programmen,\
  \ beim Start Input zu erhalten \u2013 praktisch f\xFCr flexible Tools und\u2026"
lastmod: '2024-03-13T22:44:54.240361-06:00'
model: gpt-4-1106-preview
summary: "Das Lesen von Kommandozeilenargumenten erm\xF6glicht es deinen Swift-Programmen,\
  \ beim Start Input zu erhalten \u2013 praktisch f\xFCr flexible Tools und Automatisierung."
title: Lesen von Kommandozeilenargumenten
weight: 23
---

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
