---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Kotlin: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist eine gängige Aufgabe für Programmierer. Durch die Verwendung dieser Funktion können wir sicherstellen, dass unser Code ordnungsgemäß auf die benötigten Dateien und Verzeichnisse zugreifen kann. Es hilft auch dabei, Fehler zu vermeiden und die Stabilität unserer Anwendungen zu gewährleisten.

## Wie geht's?
Kotlin bietet die `File.exists()` Funktion, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Wir können sie wie folgt verwenden:
```Kotlin
val directory = File("Pfad/zu/Verzeichnis")
if (directory.exists()) {
    println("Das Verzeichnis existiert.")
} else {
    println("Das Verzeichnis existiert nicht.")
}
```
Die Ausgabe hängt davon ab, ob das Verzeichnis vorhanden ist oder nicht. Diese Methode ist sehr einfach und leicht zu verstehen.

## Tiefergehende Informationen
Das Überprüfen der Existenz von Verzeichnissen ist eine gängige Aufgabe, die in der Programmierung seit Langem verwendet wird. Früher waren nur wenige Programmiersprachen in der Lage, dies direkt zu tun. Stattdessen mussten Entwickler komplexe Algorithmen schreiben, um zu überprüfen, ob ein Verzeichnis vorhanden ist oder nicht. Mit der Entwicklung moderner Programmiersprachen wie Kotlin ist dies jetzt viel einfacher geworden.

Alternativ können wir auch `File.isDirectory()` verwenden, um speziell zu überprüfen, ob es sich um ein Verzeichnis handelt. Dies kann nützlich sein, wenn wir sicherstellen wollen, dass es sich bei der angegebenen Datei tatsächlich um ein Verzeichnis handelt.

Die `File.exists()` Funktion verwendet intern das Dateisystem, um zu überprüfen, ob ein Verzeichnis vorhanden ist. Sie können auch die `File.isDirectory()` Funktion verwenden, die auf mehreren Plattformen verfügbar ist, einschließlich Android.

## Siehe auch
- [Kotlin Standardbibliothek - File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [So überprüfen Sie, ob ein Verzeichnis in Kotlin existiert](https://www.techiedelight.com/check-directory-exists-kotlin/)