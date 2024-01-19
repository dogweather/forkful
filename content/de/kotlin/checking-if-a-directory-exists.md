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

## Was & Warum?
Das Prüfen, ob ein Verzeichnis existiert, bezieht sich auf das Überprüfen der Anwesenheit eines spezifischen Dateipfades in Ihrem Dateisystem. Dies ist nützlich, um Fehler zu vermeiden, die auftreten, wenn Sie versuchen, Dateien zu lesen oder zu schreiben, die nicht existieren.

## So geht's:
Ein einfaches Beispiel in Kotlin sieht so aus:

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/Pfad/zum/Verzeichnis")

    if (Files.exists(path))
        println("Das Verzeichnis existiert.")
    else
        println("Das Verzeichnis existiert nicht.")
}
```

Laufen Sie das Programm und Ihre Ausgabe wird entweder "Das Verzeichnis existiert." oder "Das Verzeichnis existiert nicht." sein, abhängig davon, ob der spezifizierte Pfad existiert.

## Vertiefung:
Historisch gesehen basiert diese Methode auf der Unix-Shell-Funktion `test -d`, die auf ein Verzeichnis testet. In Kotlin werden jedoch moderne Java NIO-Bibliotheken häufiger verwendet. Es gibt auch alternative Methoden, wie zum Beispiel die Verwendung der `File`-Klasse:

```Kotlin
import java.io.File

fun main() {
    val file = File("/Pfad/zum/Verzeichnis")
    
    if(file.exists() && file.isDirectory)
        println("Das Verzeichnis existiert.")
    else
        println("Das Verzeichnis existiert nicht.")
}
```

Während das Ergebnis dasselbe ist, ist die Java NIO Methode in den meisten Fällen effizienter und bietet mehr Kontrolle über die Fehlerbehandlung.

## Siehe auch:
- JDK API Dokumentation zu Files.exists(): https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-
- JDK API Dokumentation zu File.exists(): https://docs.oracle.com.javase/8/docs/api/java/io/File.html#exists--