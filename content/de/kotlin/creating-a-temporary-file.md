---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein temporäre Datei ist ein begrenzter Speicher, der von einem Programm während seiner Ausführung verwendet wird. Programmierer nutzen diese, um Zwischenergebnisse zu speichern, die Arbeitslast zu reduzieren oder um Daten während eines Crashs nicht zu verlieren.

## Wie macht man das

Der untenstehende Code zeigt, wie man in Kotlin temporäre Dateien erstellt.

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile(prefix = "tempFile", suffix = ".txt")
    tempFile.writeText("Das ist ein Test!")
    println(tempFile.readText())
    tempFile.deleteOnExit()
}
```

Um das zu testen, fügen Sie einfach diesen Code in Ihr Programm ein und führen Sie es aus. Der Ausdruck "Das ist ein Test!" wird auf der Konsole angezeigt.

## Tiefere Einblicke

Historisch gesehen waren temporäre Dateien schon immer ein kritischer und notwendiger Bestandteil jedes Betriebssystems. Ohne sie könnten viele Funktionen und Prozesse nicht effektiv ausgeführt werden.

In Bezug auf Alternativen könnte man anstelle von temporären Dateien auf In-Memory-Datenbanken wie Redis oder SQLite zur Laufzeit Daten speicherung verwenden. Das hängt natürlich von Ihren genauen Anwendungsfällen und Anforderungen ab.

Also, wie funktioniert das wirklich in Kotlin? Das `createTempFile` ist eigentlich eine JVM-Standardfunktion, die durch den Kotlin-Wrapper aufgerufen wird. Dabei wird ein zufälliger, eindeutiger Dateiname generiert, der mit dem angegebenen Präfix und Suffix ergänzt wird. Die Datei wird im Standard-Temp-Verzeichnis des Betriebssystems erstellt, wenn kein spezifisches Verzeichnis angegeben wird.

## Siehe auch

Für weitere Informationen über Kotlin und temporäre Dateien, verweisen wir auf die folgenden Links:

- Offizielle Kotlin-Dokumentation: [http://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html](http://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- Benutzung von temporären Dateien in Java: [https://www.baeldung.com/java-io-temporary-files](https://www.baeldung.com/java-io-temporary-files)