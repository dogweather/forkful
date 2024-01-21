---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:26.674346-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente sind die Parameter, die beim Starten eines Programms über die Konsole übergeben werden. Sie ermöglichen es, dass Programme dynamisch auf unterschiedliche Eingaben reagieren und sind essentiell für skriptartige oder batchverarbeitende Anwendungen.

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