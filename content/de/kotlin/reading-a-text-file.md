---
title:                "Kotlin: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Textdateien sind häufige Formate für die Speicherung und Übertragung von Daten. Als Entwickler kann es sehr nützlich sein, Textdateien zu lesen, um auf die darin enthaltenen Informationen zuzugreifen und sie in unseren Programmen zu verwenden. In diesem Blogbeitrag werden wir uns ansehen, wie man in Kotlin eine Textdatei lesen kann.

# So geht's

In Kotlin gibt es verschiedene Möglichkeiten, um eine Textdatei zu lesen. Eine davon ist die Verwendung der `readText()` Funktion. Diese Funktion liest den Inhalt der Datei in eine Zeichenfolge und gibt sie zurück. Wir können dies in einem einfachen Beispiel demonstrieren:

```Kotlin
val text = File("example.txt").readText()
println(text)

// Ausgabe:
// Dies ist ein Beispieltext.
// Es ist immer gut, neue Dinge zu lernen.
```

Wir können auch die `forEachLine()` Funktion verwenden, um jede Zeile in der Datei zu lesen und entsprechende Aktionen durchzuführen. Hier ist ein Beispiel, in dem wir jede Zeile in der Datei ausgeben:

```Kotlin
File("example.txt").forEachLine { line ->
    println(line)
}

// Ausgabe:
// Dies ist ein Beispieltext.
// Es ist immer gut, neue Dinge zu lernen.
```

Es ist auch möglich, eine `BufferedReader` Instanz zu erstellen und damit die Datei Zeile für Zeile zu lesen. Wir können dies mit der `useLines()` Funktion tun, um sicherzustellen, dass die Datei ordnungsgemäß geschlossen wird, sobald wir damit fertig sind:

```Kotlin
File("example.txt").useLines { lines ->
    lines.forEach { line ->
        println(line)
    }
}

// Ausgabe:
// Dies ist ein Beispieltext.
// Es ist immer gut, neue Dinge zu lernen.
```

# Deep Dive

Beim Lesen von Textdateien gibt es einige wichtige Dinge zu beachten. Beispielsweise müssen wir sicherstellen, dass die Datei existiert, bevor wir versuchen, sie zu lesen, um Fehler zu vermeiden. Wir können dies mit der `exists()` Funktion überprüfen. Außerdem sollten wir sicherstellen, dass die Datei ordnungsgemäß geschlossen wird, sobald wir sie gelesen haben, um Ressourcenlecks zu vermeiden. Dafür können wir die `use` Funktion verwenden, wie im folgenden Beispiel gezeigt:

```Kotlin
File("example.txt").useLines { lines ->
    lines.forEach { line ->
        println(line)
    }
}

// Datei wird automatisch geschlossen, sobald die `useLines` Funktion beendet ist.
```

Wir können auch angeben, in welchem Zeichensatz die Datei gelesen werden soll, indem wir den entsprechenden `charset` Parameter angeben. Standardmäßig verwendet Kotlin UTF-8. Und schließlich sollten wir auch Fehler beim Lesen der Datei behandeln, indem wir `try/catch` Blöcke verwenden.

# Siehe auch

- Die offizielle Dokumentation zu [`readText()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/read-text.html)
- Die offizielle Dokumentation zu [`forEachLine()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/for-each-line.html)
- Die offizielle Dokumentation zu [`useLines()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-input-stream/use-lines.html)