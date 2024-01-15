---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Kotlin: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Manchmal benötigt man in der Programmierung die Möglichkeit zu prüfen, ob ein bestimmtes Verzeichnis auf dem Computer existiert. Dies kann hilfreich sein, um beispielsweise sicherzustellen, dass eine Datei in dem gewünschten Verzeichnis vorhanden ist, bevor man versucht, darauf zuzugreifen.

# Wie geht das?

Die Überprüfung, ob ein Verzeichnis existiert, kann relativ einfach mit Hilfe von Kotlin durchgeführt werden. Zunächst muss ein File-Objekt erstellt werden, das das zu überprüfende Verzeichnis darstellt. Anschließend kann die Funktion `exists()` aufgerufen werden, um zu überprüfen, ob das Verzeichnis tatsächlich existiert. Hier ist ein Beispielcode:

```Kotlin
val directory = File("/Pfad/zum/Verzeichnis")
if(directory.exists()){
    println("Das Verzeichnis existiert.")
} else {
    println("Das Verzeichnis existiert nicht.")
}
```

Die Ausgabe dieses Codes hängt davon ab, ob das Verzeichnis existiert oder nicht. Wenn das Verzeichnis existiert, wird die erste Nachricht ausgegeben, andernfalls die zweite. Es ist auch möglich, zusätzliche Logik hinzuzufügen, um beispielsweise entsprechend zu handeln, falls das Verzeichnis nicht existiert.

# Tiefer eintauchen

Wenn man tiefer in das Thema einsteigen möchte, gibt es weitere Möglichkeiten, um zu überprüfen, ob ein Verzeichnis existiert. So kann man beispielsweise auch die Funktion `isDirectory()` verwenden, um zu überprüfen, ob es sich bei dem angegebenen Pfad tatsächlich um ein Verzeichnis handelt. Außerdem gibt es auch spezielle Funktionen, um zu überprüfen, ob ein Verzeichnis schreibgeschützt, ausführbar oder lesbar ist.

In einigen Fällen kann es auch nützlich sein, zu überprüfen, ob ein Verzeichnis leer ist. Hierfür kann die Funktion `listFiles()` verwendet werden, die eine Liste mit allen Dateien und Verzeichnissen in dem angegebenen Pfad zurückgibt. Ist diese Liste leer, kann davon ausgegangen werden, dass das Verzeichnis selbst auch leer ist.

# Siehe auch

- [Offizielle Kotlin Dokumentation zu File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Artikel zur Überprüfung von Verzeichnissen in Java](https://www.geeksforgeeks.org/check-if-a-file-is-a-directory-or-a-file-in-java/) (auf Englisch)
- [Beispiele zu File Operationen mit Kotlin](https://www.baeldung.com/java-file-directory-exists) (auf Englisch)