---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Java: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Verzeichnisüberprüfung in Java bedeutet, dass man ermittelt, ob ein spezifisches Verzeichnis (ein ‚Directory’) auf dem Dateisystem vorhanden ist oder nicht. Wir machen dieses, um typische Fehler zu vermeiden und effektivere Programme zu erstellen, zum Beispiel um sicherzustellen, dass das Verzeichnis vorhanden ist, bevor wir Dateien darin erstellen oder lesen.

## So geht's:

Wir müssen nur die Methode `exists()` und `isDirectory()` der in Java verfügbaren Klasse ‚java.nio.file.Files‘ verwenden.

```Java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        Path path = Paths.get("/pfad/zum/verzeichnis");

        if (Files.exists(path) && Files.isDirectory(path)) {
            System.out.println("Das Verzeichnis existiert!");
        } 
        else {
            System.out.println("Das Verzeichnis existiert nicht!");
        }
    }
}
```

Wenn das angegebene Verzeichnis existiert, erhalten Sie folgenden Ausdruck: "Das Verzeichnis existiert!". Falls nicht, erhalten Sie: "Das Verzeichnis existiert nicht!".

## Tiefgang:

Historisch gesehen hat sich die Art und Weise, wie wir in Java überprüfen, ob ein Verzeichnis existiert, mit den Updates der Java-Versionen geändert. In älteren Versionen von Java (vor Java 7) wurde das `java.io.File` Objekt anstelle der `java.nio.file.Files` Klasse verwendet.

Es gibt auch andere Wege, wie man ein Verzeichnis in Java überprüfen kann. Einige APIs von Drittanbietern wie Apache Commons IO und Guava bieten Funktionen, die das gleiche Ziel erreichen können. Trotzdem bietet die eingebaute und einheitliche `java.nio.file.Files` Methode meistens die einfachste und effizienteste Lösung.

Die Methode `Files.exists()` überprüft, ob der Pfad existiert und die Methode `Files.isDirectory()` überprüft, ob es sich um ein Verzeichnis und nicht um eine Datei handelt.

## Siehe auch:

1. Java 7-Dateidokumentation: https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html
2. Java-Pfade: https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html
3. Apache Commons IO: https://commons.apache.org/proper/commons-io/
4. Google-Guava-Bibliothek: https://github.com/google/guava