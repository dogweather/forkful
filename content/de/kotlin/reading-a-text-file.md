---
title:                "Einen Textdatei lesen."
html_title:           "Kotlin: Einen Textdatei lesen."
simple_title:         "Einen Textdatei lesen."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich für das Programmieren mit Kotlin interessierst und Dateien verwenden möchtest, ist es wichtig zu wissen, wie du Textdateien lesen und verarbeiten kannst. Diese Fähigkeit ist für viele Anwendungsfälle unerlässlich und wird dir helfen, effektivere und vielfältigere Programme zu schreiben.

## Wie geht man vor?

Die Grundlage jeder Dateiverarbeitung ist das Öffnen der Datei, um den darin enthaltenen Text abzurufen. Dafür gibt es in Kotlin die Methode `readText()` für `File`-Objekte. Wir können also zunächst ein `File`-Objekt erstellen und dieses dann mit der entsprechenden Methode öffnen:

```Kotlin
val file = File("pfad/zur/datei.txt")
val text = file.readText()
```

Jetzt haben wir den gesamten Inhalt der angegebenen Textdatei in der Variable `text` gespeichert. Dies könnten wir zum Beispiel in der Konsole ausgeben:

```Kotlin
println(text)
```

Du kannst auch zeilenweise durch die Datei iterieren und jeden Textabschnitt einzeln verarbeiten. Dafür gibt es die Methode `forEachLine()`:

```Kotlin
file.forEachLine {
    // hier kannst du den jeweiligen Textabschnitt verarbeiten
}
```

## Deep Dive

Bei der Verarbeitung von Textdateien ist es wichtig zu beachten, dass verschiedene Dateiformate auch unterschiedliche Zeichenkodierungen haben können. Um sicherzustellen, dass du den Text korrekt interpretierst, kannst du die Methode `readText(charset: Charset)` verwenden und die gewünschte Kodierung angeben. Außerdem ist es empfehlenswert, beim Öffnen oder Lesen von Dateien immer mit Exceptions umzugehen, um unerwartete Fehler abzufangen und angemessen zu reagieren.

## Siehe auch

- [Dokumentation zu Dateiverarbeitung in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tutorial zu Dateizugriff in Kotlin](https://www.baeldung.com/kotlin-reading-file)
- [Beispiele für Kotlin-Code zur Dateiverarbeitung](https://www.programiz.com/kotlin-programming/file-handling)