---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:57:06.552233-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Überprüfen, ob ein Verzeichnis existiert, heißt herauszufinden, ob ein bestimmter Ordnerpfad auf dem Datenträger vorhanden ist. Programmierer machen das, um Laufzeitfehler zu vermeiden, die auftreten können, wenn sie versuchen, Dateioperationen in einem nicht vorhandenen Verzeichnis durchzuführen.

## Vorgehensweise:
```kotlin
import java.nio.file.*

fun main() {
    val path = Paths.get("/mein/verzeichnis/pfad")

    if (Files.exists(path)) {
        println("Das Verzeichnis existiert.")
    } else {
        println("Das Verzeichnis existiert nicht.")
    }
}
```

Sample Output:
```
Das Verzeichnis existiert.
```
oder
```
Das Verzeichnis existiert nicht.
```

## Tiefergehende Informationen:
Früher, in Java, war das Überprüfen von Verzeichnissen etwas umständlicher und weniger zuverlässig. Mit der Einführung von NIO (New I/O) in Java 7, welche dann auch ihren Weg in die Kotlin Standardbibliothek gefunden hat, haben Entwickler nun eine robustere Methode, um das Vorhandensein von Dateien und Verzeichnissen zu prüfen. Eine Alternative wäre die `File`-Klasse zu verwenden, aber `Files.exists()` ist eindeutiger, weil man mit `File` zwischen nicht existierenden Verzeichnissen und Zugriffsproblemen nicht unterscheiden kann. Die `Files.exists()`-Methode kann auch erweitert werden, um die Lesbarkeit oder Zugriffsrechte eines Verzeichnisses zu prüfen, was beim Management von Dateisystemoperationen nützlich ist.

## Siehe auch:
- [Java NIO File Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Vergleich zwischen File und Path in Java](https://www.baeldung.com/java-file-directory-exists)