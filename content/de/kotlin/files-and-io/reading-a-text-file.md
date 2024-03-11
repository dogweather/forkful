---
date: 2024-01-20 17:54:51.447948-07:00
description: "Textdateien lesen hei\xDFt, Daten aus einer Datei mit Textinhalt in\
  \ dein Programm zu laden. Programmierer machen das h\xE4ufig, um Konfigurationen\
  \ auszulesen,\u2026"
lastmod: '2024-03-11T00:14:27.757591-06:00'
model: gpt-4-1106-preview
summary: "Textdateien lesen hei\xDFt, Daten aus einer Datei mit Textinhalt in dein\
  \ Programm zu laden. Programmierer machen das h\xE4ufig, um Konfigurationen auszulesen,\u2026"
title: Textdatei einlesen
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien lesen heißt, Daten aus einer Datei mit Textinhalt in dein Programm zu laden. Programmierer machen das häufig, um Konfigurationen auszulesen, Benutzerdaten zu verarbeiten oder einfach Informationen zu speichern und wiederzuverwenden.

## How to:
So liest du eine Textdatei in Kotlin – einfach und schmerzlos:

```Kotlin
import java.io.File

fun main() {
    val content = File("meineDatei.txt").readText(Charsets.UTF_8)
    println(content)
}

```
Ergebnis:
```
Hallo, das ist der Inhalt der Textdatei!
```

## Deep Dive
Der Umgang mit Dateien in Kotlin beruht auf Java's I/O-API. `java.io.File` ist seit Java 1.0 dabei – ein Oldtimer. `readText()` ist eine bequeme Methode, die erst mit Kotlin kam. Sie liest den ganzen Inhalt auf einmal, super für kleine Dateien, aber Vorsicht bei großen – Speicherprobleme drohen.

Alternativ kannst du mit `readLines()` jede Zeile in eine Liste lesen, das hält den Speicherbedarf klein:

```Kotlin
val lines = File("meineDatei.txt").readLines()
lines.forEach { println(it) }
```

Was, wenn dein Datei riesig ist? `bufferedReader()` zur Hilfe:

```Kotlin
File("großeDatei.txt").bufferedReader().use { reader ->
    var line = reader.readLine()
    while(line != null) {
        println(line)
        line = reader.readLine()
    }
}
```
Hier haben wir einen `BufferedReader`, der Stück für Stück liest, statt alles auf einmal in den RAM zu hauen.

Das Wichtigste? Fehlerbehandlung nicht vergessen. Schau Dir `try-catch`-Blöcke an, um den Umgang mit fehlenden Dateien oder Zugriffsproblemen elegant zu handhaben.

## See Also
- Offizielles Kotlin Forum für Fragen: [Kotlin Discussions](https://discuss.kotlinlang.org/)
