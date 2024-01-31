---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:54.362550-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wir überprüfen, ob ein Verzeichnis existiert, um sicherzustellen, dass unsere Programme mit den Dateien und Ordnern arbeiten können, die sie benötigen. Wenn wir wissen, ob ein Verzeichnis vorhanden ist, können wir Fehler vermeiden und gegebenenfalls neue Verzeichnisse erstellen.

## How to:
Hier ist ein einfaches Beispiel, wie man in Java prüft, ob ein Verzeichnis existiert:

```java
import java.nio.file.*;

public class DirectoryCheck {
    public static void main(String[] args) {
        Path directoryPath = Paths.get("/path/to/directory");

        if (Files.exists(directoryPath)) {
            System.out.println("Das Verzeichnis existiert!");
        } else {
            System.out.println("Das Verzeichnis existiert nicht!");
        }
    }
}
```
Ausgabe, je nachdem, ob das Verzeichnis existiert oder nicht:
```
Das Verzeichnis existiert!
```
oder
```
Das Verzeichnis existiert nicht!
```

## Deep Dive:
Die Überprüfung, ob ein Verzeichnis existiert, ist ein Standardvorgang seit den ersten Tagen der Dateisysteminteraktion über eine Programmierschnittstelle. In Java haben sich über die Jahre Methoden entwickelt, angefangen bei `File.exists()` bis hin zu `Files.exists()` aus dem `java.nio`-Paket, welches ab Java 7 mit dem New I/O 2 (auch bekannt als NIO.2) eingeführt wurde. Es bietet eine bessere Leistung und ist flexibler als der alte `java.io`-Ansatz.

Es gibt auch Methoden wie `Files.notExists()`, die genutzt werden können, um ausdrücklich zu prüfen, ob ein Verzeichnis nicht existiert. Manchmal kann es sinnvoll sein, `Files.isDirectory()` zu verwenden, um sicherzustellen, dass der Pfad nicht nur existiert, sondern auch ein Verzeichnis ist.

Bei allen diesen Möglichkeiten ist jedoch Vorsicht geboten – die Überprüfung des Vorhandenseins eines Verzeichnisses kann je nach Dateisystem und Berechtigungen unterschiedlich schnell sein und das Ergebnis kann sich ändern, sobald es nachfolgende Dateisystemänderungen gibt.

## See Also:
- Die offizielle Java-Dokumentation zu `Files.exists()`: [Files.exists()](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...))
- Das Java Tutorials - Working with Files: [Java Tutorials - Files](https://docs.oracle.com/javase/tutorial/essential/io/)
