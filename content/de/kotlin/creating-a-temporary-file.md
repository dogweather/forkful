---
title:                "Erstellen einer temporären Datei"
html_title:           "Kotlin: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Die Erstellung temporärer Dateien ist ein gängiges Problem in der Softwareentwicklung. Es kann nützlich sein, um vorübergehende Daten zu speichern, die nur für eine bestimmte Zeit benötigt werden, oder um sicherzustellen, dass die Daten des Benutzers nicht dauerhaft gespeichert werden.

## Wie es geht

Die Erstellung einer temporären Datei in Kotlin ist sehr einfach. Hier sind einige Beispiele, die zeigen, wie man eine temporäre Datei erstellen und speichern kann:

```Kotlin
// Erstellen einer temporären Datei mit Standardpräfix und Suffix
val tempFile = File.createTempFile("temp", ".txt")
tempFile.deleteOnExit() // Datei automatisch löschen, wenn das Programm beendet wird

// Erstellen einer temporären Datei im angegebenen Verzeichnis
val tempFile = File.createTempFile("temp", ".txt", File("C:/Temp"))

// Erstellen einer temporären Datei mit benutzerdefiniertem Präfix und Suffix
val tempFile = File.createTempFile("logs-", "-backup", File("C:/Logs"))
```

Das Ergebnis wird jedes Mal eine neue temporäre Datei mit einem eindeutigen Namen sein. Wenn die Datei automatisch gelöscht werden soll, kann die Methode `deleteOnExit()` aufgerufen werden.

## Tiefergehende Informationen

Bei der Erstellung einer temporären Datei werden viele Faktoren berücksichtigt, wie z.B. das Betriebssystem, das Dateisystem und die aktuelle Benutzerberechtigung. Es ist wichtig zu beachten, dass diese Dateien nicht dauerhaft gespeichert werden und nicht für langfristige Speicherung gedacht sind.

Zusätzlich kann eine temporäre Datei mit weiteren Eigenschaften versehen werden, wie z.B. Berechtigungen, Datum und Uhrzeit der Erstellung oder auch der Länge der Datei. All diese Faktoren können in der `createTempFile()` Methode angepasst werden.

## Siehe auch

- [Kotlin Dokumentation für File-Klasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java-Tutorial zum Erstellen temporärer Dateien](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))