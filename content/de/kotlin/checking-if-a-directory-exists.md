---
title:    "Kotlin: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der Programmierung. Es ermöglicht uns zu prüfen, ob bestimmte Dateien oder Ordner vorhanden sind, bevor wir versuchen, mit ihnen zu arbeiten. Dies hilft uns, Fehler zu vermeiden und einen zuverlässigen Code zu schreiben.

## Wie man es macht

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Funktion `exists()` aus der `File`-Klasse verwenden. Diese Funktion gibt uns einen booleschen Wert zurück, der angibt, ob das Verzeichnis existiert oder nicht.

```Kotlin
val directory = File("/pfad/zum/verzeichnis")
if (directory.exists()) {
    println("Das Verzeichnis existiert!")
} else {
    println("Das Verzeichnis existiert nicht!")
}
```

In diesem Beispiel erstellen wir ein `File`-Objekt für das angegebene Verzeichnis und rufen dann die `exists()`-Funktion auf. Wenn das Verzeichnis existiert, wird die entsprechende Meldung ausgegeben, ansonsten wird die andere Meldung angezeigt.

## Tiefere Einblicke

Bevor wir ein Verzeichnis überprüfen, müssen wir sicherstellen, dass wir den richtigen Pfad dafür angeben. Wir können dies tun, indem wir die Funktion `isDirectory()` aus der `File`-Klasse verwenden. Diese Funktion gibt uns ebenfalls einen booleschen Wert zurück und überprüft, ob die angegebene Datei tatsächlich ein Verzeichnis ist.

Ein weiterer wichtiger Punkt ist, dass die `exists()`-Funktion lediglich prüft, ob das Verzeichnis da ist, jedoch nicht, ob wir Zugriff auf das Verzeichnis haben. Wir müssen also auch sicherstellen, dass wir die erforderlichen Berechtigungen haben, um auf das Verzeichnis zuzugreifen.

## Siehe auch

- [Java Dokumentation zu File.exists()](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())
- [Kotlin Dokumentation zu File.isDirectory()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/is-directory.html)