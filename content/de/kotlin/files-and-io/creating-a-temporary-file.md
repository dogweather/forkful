---
date: 2024-01-20 17:41:00.642193-07:00
description: "How to: Kotlin bietet einen simplen Weg, tempor\xE4re Dateien zu erstellen.\
  \ Hier siehst du, wie's geht."
lastmod: '2024-03-13T22:44:53.865626-06:00'
model: gpt-4-1106-preview
summary: "Kotlin bietet einen simplen Weg, tempor\xE4re Dateien zu erstellen."
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

## How to:
Kotlin bietet einen simplen Weg, temporäre Dateien zu erstellen. Hier siehst du, wie's geht:

```Kotlin
import java.io.File

fun main() {
    // Temporäre Datei erstellen
    val tempFile = File.createTempFile("temp", ".tmp")
    println("Temporäre Datei wurde erstellt: ${tempFile.absolutePath}")

    // Daten in die temporäre Datei schreiben
    tempFile.writeText("Das ist ein Test!")

    // Datei lesen und ausgeben
    val content = tempFile.readText()
    println("Inhalt der temporären Datei: $content")

    // Temporäre Datei am Ende löschen
    tempFile.deleteOnExit()
    println("Temporäre Datei wird nach Programmende gelöscht.")
}
```
Output:
```
Temporäre Datei wurde erstellt: /tmp/temp1234567890.tmp
Inhalt der temporären Datei: Das ist ein Test!
Temporäre Datei wird nach Programmende gelöscht.
```

## Deep Dive
Temporäre Dateien gibt's so lange, wie es Betriebssysteme gibt. Der Trick ist, dass sie bei Neustart des Systems oder bei Beendigung der App, die sie erzeugt hat, verschwinden sollen. Java und Kotlin nutzen dafür eine einfache API, welche temporäre Dateien im Standard-Temp-Verzeichnis des Betriebssystems erstellt.

Es gibt Alternativen wie das manuelle Erstellen von Dateien mit Zufallsnamen. Aber wozu das Rad neu erfinden? Kotlin und Java nehmen einem hier die Arbeit ab. Ein wichtiger Aspekt beim Umgang mit temporären Dateien ist das saubere Aufräumen. `deleteOnExit()` ist hilfreich, aber es könnte sein, dass man die Kontrolle über das Löschen sofort haben will. In diesem Fall benutzt `tempFile.delete()` sofort nach Gebrauch.

Unter der Haube passt das JVM (Java Virtual Machine) auf, dass alles richtig funktioniert. Es sorgt dafür, dass die Dateien eindeutig identifizierbar sind und nicht von anderen Prozessen zufällig benutzt werden.

## See Also
- Kotlin Dokumentation zu Dateien: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/) 
- Java File API: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Temporäre Dateien in Java: [https://docs.oracle.com/javase/tutorial/essential/io/fileio.html#temp](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html#temp)
