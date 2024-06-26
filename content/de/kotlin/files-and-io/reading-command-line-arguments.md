---
date: 2024-01-20 17:56:26.674346-07:00
description: "How to: Um in Kotlin Kommandozeilenargumente zu lesen, greift man einfach\
  \ auf das Array `args` zu, das in die `main`-Funktion eingebettet ist. Sieht so\u2026"
lastmod: '2024-03-13T22:44:53.861513-06:00'
model: gpt-4-1106-preview
summary: Um in Kotlin Kommandozeilenargumente zu lesen, greift man einfach auf das
  Array `args` zu, das in die `main`-Funktion eingebettet ist.
title: Lesen von Kommandozeilenargumenten
weight: 23
---

## How to:
Um in Kotlin Kommandozeilenargumente zu lesen, greift man einfach auf das Array `args` zu, das in die `main`-Funktion eingebettet ist. Sieht so aus:

```kotlin
fun main(args: Array<String>) {
    args.forEachIndexed { index, arg ->
        println("Argument $index: $arg")
    }
}

// Ausgabe, wenn Befehl `kotlin MeinProgramm.kt arg1 arg2 arg3`
// Argument 0: arg1
// Argument 1: arg2
// Argument 2: arg3
```

Nutzt man `args` in Verbindung mit Bedingungen oder Schleifen, lassen sich diverse Szenarien und Parameterkombinationen abdecken.

## Deep Dive
Historisch gesehen ist das Lesen von Kommandozeilenargumenten ein Überbleibsel aus den Anfangszeiten der Softwareentwicklung und bildet auch heute noch die Basis für viele Scripts und Tools, die ohne grafische Benutzeroberfläche auskommen. 

In Kotlin sind die Argumente als Array von Strings verfügbar und ziemlich simpel zu handhaben – das macht Kotlin nebenbei zu einer guten Sprache für Kommandozeilenanwendungen. Alternativen zum direkten Auslesen wären die Nutzung von Parser-Bibliotheken wie `kotlinx.cli` oder das Konvertieren der `args` in eine Collection oder eine andere Datenstruktur, um mit ihnen komfortabler arbeiten zu können.

Das Implementieren einer eigenen Logik zur Argumentenanalyse ist reizvoll, kann aber schnell unübersichtlich werden, wenn die Anzahl der Parameter und Optionen wächst. Daher ist es oft ratsam, auf bestehende Lösungen zurückzugreifen.

## See Also:
- Die offizielle Dokumentation zu Kotlin: https://kotlinlang.org/docs/home.html
- `kotlinx-cli`, eine Kotlin-Bibliothek zum Parsen von Kommandozeilenoptionen: https://github.com/Kotlin/kotlinx-cli
- Eine Anleitung für kommandozeilengesteuerte Anwendungen in Kotlin: https://kotlinlang.org/docs/command-line.html
