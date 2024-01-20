---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Textdateien zu lesen ist ein grundlegender Prozess in der Programmierung, bei dem ein Programm Daten aus einer Textdatei einliest und verarbeitet. Dies ist nützlich, um eine Vielzahl von Aufgaben zu erfüllen, von der Konfigurationseinstellungen bis hin zur Datenanalyse.

## Anleitung:

In Kotlin lässt sich eine Textdatei mit der Funktion `readText()` sehr einfach einlesen:
```Kotlin
import java.io.File

fun main() {
    val textFromFile = File("beispiel.txt").readText()
    println(textFromFile)
}
```
Wenn "beispiel.txt" den Text "Hallo Welt" enthält, wird das Programm den gleichen Text ausgeben.

## Vertiefung:

Das Einlesen von Textdateien ist ein alter Bestandteil der Informatik und hat seine Wurzeln in den frühesten Tagen der digitalen Datenverarbeitung. Es gibt viele alternative Methoden zum Einlesen von Textdateien, zum Beispiel mit Buffern oder Streams, welche in Fällen von sehr großen Dateien nützlich sein können um den Speicherverbrauch zu minimieren. 

In Kotlin wird der `readText()`-Funktion intern ein Buffered Reader verwendet, um die Datei zeilenweise zu lesen. Diese Methode ist sehr effektiv und sicher, kann jedoch bei sehr großen Dateien zu Speicherproblemen führen. In solchen Fällen wäre es besser, einen `BufferedReader` direkt zu verwenden und die Datei zeilenweise zu lesen.

```Kotlin
import java.io.File

fun main() {
    File("beispiel.txt").bufferedReader().forEachLine { println(it) }
}
```

## Siehe auch:

- [Offizielle Kotlin-Dokumentation: Dateien lesen und schreiben](https://kotlinlang.org/docs/tutorials/kotlin-for-py/io.html)

- [stackoverflow: Wie liest man in Kotlin eine Datei?](https://stackoverflow.com/questions/47166369/how-to-read-a-text-file-in-kotlin)

Remember, practice makes perfect. Happy coding! (Erinnern Sie sich daran, Übung macht den Meister. Frohes Codieren!)