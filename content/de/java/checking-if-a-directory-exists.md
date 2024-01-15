---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Java: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil der Java-Programmierung, da es die Möglichkeit bietet, bestimmte Aktionen basierend auf der Existenz von Verzeichnissen durchzuführen. Zum Beispiel kann man prüfen, ob ein bestimmtes Verzeichnis existiert, bevor man versucht, darauf zuzugreifen oder neue Dateien darin zu erstellen. Dies kann dazu beitragen, Fehler in der Ausführung des Codes zu vermeiden und die Gesamteffizienz eines Programms zu verbessern.

## Wie geht das?

Die Überprüfung der Existenz eines Verzeichnisses in Java ist relativ einfach und erfordert nur wenige Zeilen Code. Im Folgenden finden Sie ein Beispiel, das zeigt, wie Sie diese Überprüfung mit der Methode "exists()" der Klasse "File" durchführen können:

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // Erstelle ein File-Objekt für das zu überprüfende Verzeichnis
        File directory = new File("Pfad/zum/verzeichnis");

        // Überprüfe, ob das Verzeichnis existiert
        if (directory.exists()) {
            System.out.println("Das Verzeichnis existiert.");
        } else {
            System.out.println("Das Verzeichnis existiert nicht.");
        }
    }
}
```

Ausgabe: Wenn das Verzeichnis existiert, wird die erste Zeile ausgegeben. Andernfalls wird die zweite Zeile angezeigt.

## Tiefer gehende Erläuterungen

Die Methode "exists()" der Klasse "File" gibt einen booleschen Wert zurück - true, wenn das Verzeichnis existiert, und false, wenn es nicht existiert. Es kann auch verwendet werden, um zu überprüfen, ob eine Datei oder ein Symbolischer Link existiert. Darüber hinaus kann auch die Methode "isDirectory()" verwendet werden, um zu überprüfen, ob es sich bei dem übergebenen File-Objekt tatsächlich um ein Verzeichnis handelt.

## Siehe auch

- [Java File API Dokumentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Tutorial zur Dateibearbeitung in Java](https://www.baeldung.com/java-file)