---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Textdateien schreiben bedeutet, Daten in einem lesbaren Format zu speichern. Programmierer machen das, um Konfigurationen, Logs oder Daten für spätere Verwendung zu sichern.

## So geht's:

```kotlin
import java.io.File

fun main() {
    val myText = "Hallo Welt! Dies ist ein Beispielsatz."
    File("example.txt").writeText(myText)
}
```

Ausgabe: Die Datei `example.txt` enthält nun den Text "Hallo Welt! Dies ist ein Beispielsatz."

## Deep Dive

Das Schreiben von Textdateien ist so alt wie die Programmierung selbst und basiert auf dem Betriebssystem API. Alternativ zu `java.io.File` könnten wir `java.nio` verwenden, das effizientere Methoden für I/O-Operationen bietet. Details der Implementierung hängen von der Dateigröße und der notwendigen Performance ab.

## Siehe auch:

- Kotlin Dokumentation: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- Oracle Java Docs für `java.io`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- Oracle Java Docs für `java.nio`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
