---
title:                "Kotlin: Einen Textdatei lesen"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind in der Welt der Programmierung von entscheidender Bedeutung. Sie dienen als Datenspeicher für die von uns erstellten Anwendungen. Das Lesen einer Textdatei ist ein grundlegendes Konzept, das jeder Entwickler beherrschen sollte.

## So geht's

Um eine Textdatei in Kotlin zu lesen, müssen wir zunächst den Dateipfad angeben und eine `File`-Instanz erstellen. Dann verwenden wir `BufferedReader()` und `readLine()` in einer Schleife, um die Datei zeilenweise zu lesen und sie in eine Variable zu speichern. Hier ist ein Beispielcode:

```Kotlin
val file = File("pfad/zu/datei.txt")
val reader = BufferedReader(file.reader())
var line: String? = reader.readLine()
while (line != null) {
    // Zeile in Variable `line` speichern
    println(line)
    line = reader.readLine()
}
reader.close()
```

Wenn wir beispielsweise eine Textdatei mit folgendem Inhalt haben:

```
Hallo
Welt
```

Wird die Ausgabe folgendermaßen aussehen:

```
Hallo
Welt
```

## Tiefer Einblick

Für eine genauere Betrachtung des Textdatei-Lesens in Kotlin können wir uns die `File`-Klasse genauer ansehen. Diese Klasse bietet uns verschiedene Methoden zum Lesen, Schreiben und Manipulieren von Dateien. Außerdem können wir auch die `Charset`-Option verwenden, um die Zeichenkodierung unserer Datei anzugeben.

Die Verwendung von `BufferedWriter` ist auch eine effiziente Möglichkeit, um Textdateien zu lesen, da es den Lesevorgang puffert und damit die Leistung verbessert.

## Siehe auch

- [Kotlin-Dokumentation zu Dateien](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tutorial zum Lesen von Textdateien in Kotlin](https://www.javatpoint.com/kotlin-read-text-file)
- [Codebeispiel zum Schreiben von Textdateien mit Kotlin](https://www.programiz.com/kotlin-programming/writing-files)