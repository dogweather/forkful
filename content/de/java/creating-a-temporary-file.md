---
title:                "Erstellen einer temporären Datei"
html_title:           "Java: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Warum

Das Erstellen temporärer Dateien ist in der Java-Programmierung ein häufig verwendetes Konzept. Es ermöglicht uns, temporäre Daten zu speichern, die nur für einen bestimmten Zeitraum oder für einen spezifischen Zweck benötigt werden.

## Wie geht's

Um eine temporäre Datei in Java zu erstellen, können wir die Klasse `java.io.File` verwenden. Wir müssen jedoch darauf achten, dass wir die Datei mit der Erweiterung `.tmp` erstellen, um sie als temporär zu kennzeichnen. Hier ist ein Beispielcode:

```Java
import java.io.File;

public class TempFileExample {

    public static void main(String[] args) {

        // Hier erstellen wir eine temporäre Datei
        File tempFile = new File("tempfile.tmp");

        // Zum Überprüfen, ob die Datei erfolgreich erstellt wurde
        if (tempFile.exists()) {
            System.out.println("Temporäre Datei wurde erfolgreich erstellt!");
        } else {
            System.out.println("Fehler beim Erstellen der temporären Datei!");
        }
    }
}
```
Output:
```
Temporäre Datei wurde erfolgreich erstellt!
```

## Tiefere Einblicke

Beim Erstellen einer temporären Datei können wir auch eine Option für den Speicherort angeben. Standardmäßig erstellt Java die temporäre Datei im Systemtemp-Verzeichnis, aber mit der Methode `File.createTempFile` können wir einen benutzerdefinierten Speicherort angeben. Außerdem gibt es in Java auch die Möglichkeit, temporäre Dateien mit einer bestimmten Präfix- und Suffix-Option zu erstellen, um sie besser zu identifizieren.

## Siehe auch

- [Java Dokumentation: Klasse java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Wie man temporäre Dateien in Java erstellt](https://www.baeldung.com/java-temporary-files)