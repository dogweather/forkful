---
title:                "Erstellung einer temporären Datei"
date:                  2024-01-20T17:40:21.827520-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Temporäre Dateien sind kurzlebige Dateispeicher, die während der Laufzeit eines Programms erstellt werden. Programmierer nutzen sie, um Daten zwischenzuspeichern, ohne langfristigen Speicherplatz zu belegen, oder um sensitive Daten zu handhaben, die nicht dauerhaft gespeichert werden sollen.

## How to:
```java
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

public class TempFileBeispiel {
    public static void main(String[] args) {
        try {
            // Erstellen einer temporären Datei
            File tempFile = Files.createTempFile("meineTempDatei", ".txt").toFile();
            System.out.println("Temporäre Datei wurde erstellt: " + tempFile.getAbsolutePath());

            // Beachten: Temporäre Dateien müssen manuell gelöscht werden
            tempFile.deleteOnExit();
            System.out.println("Temporäre Datei wird nach Programmende gelöscht.");

        } catch (IOException e) {
            System.out.println("Ein Fehler ist aufgetreten: " + e.getMessage());
        }
    }
}
```
Sample Output:
```
Temporäre Datei wurde erstellt: /tmp/meineTempDatei1234567890.txt
Temporäre Datei wird nach Programmende gelöscht.
```

## Deep Dive
Bevor die `java.nio`-Paketfamilie eingeführt wurde, war das Erstellen temporärer Dateien weniger intuitiv. Man nutzte `File.createTempFile`, aber mit `java.nio.file.Files` ist es jetzt einfacher und sauberer. Alternativ gibt es Bibliotheken wie Apache Commons IO, die ähnliche Funktionalitäten bieten. Bei der Implementierung ist es wichtig zu bedenken, dass temporäre Dateien sicherheitsrelevante Aspekte haben können, besonders, wenn sie sensible Daten enthalten. In solchen Fällen sollte man sicherstellen, dass die Dateien ordnungsgemäß gelöscht oder mit entsprechenden Sicherheitsmechanismen gehandhabt werden.

## See Also
- [The official Java Tutorials for Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java Documentation for Files.createTempFile](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#createTempFile-java.lang.String-java.lang.String-java.nio.file.attribute.FileAttribute...-)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
