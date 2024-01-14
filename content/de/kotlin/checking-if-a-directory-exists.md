---
title:                "Kotlin: Überprüfen, ob ein Verzeichnis existiert."
simple_title:         "Überprüfen, ob ein Verzeichnis existiert."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Die Überprüfung, ob ein Verzeichnis existiert, ist eine wichtige Fähigkeit, die jeder Entwickler lernen sollte. Dies ermöglicht es uns, effektiv mit Dateien und Ordnern auf unserem Computer zu arbeiten und unsere Anwendung entsprechend anzupassen.

# Wie man es macht

Die Überprüfung, ob ein Verzeichnis existiert, kann auf verschiedene Arten erfolgen. Hier ist ein Beispiel in Kotlin:

```Kotlin
import java.io.File

fun main() {
    val directory = File("Pfad/zum/Verzeichnis")
    if (directory.exists()) {
        println("Das Verzeichnis existiert.")
    } else {
        println("Das Verzeichnis existiert nicht.")
    }
}
```

Die Ausgabe des obigen Codes wäre "Das Verzeichnis existiert.", wenn das Verzeichnis tatsächlich existiert. Andernfalls würde die Ausgabe "Das Verzeichnis existiert nicht." lauten.

# Tiefere Einblicke

Um zu verstehen, wie die Überprüfung funktioniert, müssen wir uns die Verwendung der Klasse File in Kotlin ansehen. Die Methode `exists()` überprüft, ob das Objekt tatsächlich auf dem Dateisystem vorhanden ist. Hier sind einige andere Methoden, die wir verwenden können, um zu bestimmen, ob ein Verzeichnis oder eine Datei existiert:

- `isDirectory()`: Überprüft, ob das Objekt ein Verzeichnis ist.
- `isFile()`: Überprüft, ob das Objekt eine Datei ist.
- `canRead()`: Überprüft, ob das Objekt lesbar ist.
- `canWrite()`: Überprüft, ob das Objekt beschreibbar ist.
- `canExecute()`: Überprüft, ob das Objekt ausführbar ist.

Diese Methoden können uns helfen, verschiedene Bedingungen zu überprüfen und entsprechend auf sie zu reagieren.

# Siehe auch

- [Offizielle Kotlin-Dokumentation zur Klasse File] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Tutorial zur Arbeit mit Dateien und Verzeichnissen in Kotlin] (https://www.tutorialspoint.com/kotlin/kotlin_files_hashmap.htm)
- [OverAPI-Kotlin-Referenz] (https://overapi.com/kotlin)