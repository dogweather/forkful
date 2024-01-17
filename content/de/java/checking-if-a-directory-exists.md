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
Beim Programmieren müssen wir manchmal überprüfen, ob ein Verzeichnis existiert. Das bedeutet, dass wir prüfen, ob ein bestimmter Ordner in unserem Dateisystem vorhanden ist. Wir setzen diese Überprüfung ein, um sicherzustellen, dass unser Programm die benötigten Dateien finden und verarbeiten kann.

## Wie geht's?
Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Methode `exists()` aus der Java `File` Klasse verwenden. Diese Methode gibt `true` zurück, wenn das angegebene Verzeichnis existiert, ansonsten gibt sie `false` zurück.

```Java
File directory = new File("Pfad/zum/Verzeichnis");

if (directory.exists()) {
    System.out.println("Das Verzeichnis existiert.");
} else {
    System.out.println("Das Verzeichnis existiert nicht.");
}
```

Die Ausgabe in diesem Beispiel würde je nach Existenz des Verzeichnisses entweder `Das Verzeichnis existiert.` oder `Das Verzeichnis existiert nicht.` sein.

## Tiefergehende Informationen
Die Überprüfung der Existenz eines Verzeichnisses ist eine wichtige Aufgabe beim Programmieren. Früher wurde dies häufig mithilfe der `File.isDirectory()` Methode durchgeführt, die jedoch mittlerweile als veraltet gilt. Stattdessen wird empfohlen, die `exists()` Methode zu verwenden.

Alternativ zur `File` Klasse können Sie auch die `Path` Klasse aus dem `java.nio` Paket verwenden. Hierbei können Sie die Methode `Files.exists()` nutzen, um die Existenz eines Verzeichnisses zu prüfen.

Eine wichtige Sache, die beim Überprüfen der Existenz eines Verzeichnisses zu beachten ist, ist, dass dies keine garantierte Operation ist. Es kann vorkommen, dass das Verzeichnis zwischen dem Prüfen und dem tatsächlichen Zugriff auf das Verzeichnis gelöscht wird. Es ist daher wichtig, auch beim Zugriff auf das Verzeichnis entsprechende Fehlerbehandlung zu implementieren.

## Siehe auch
- [Oracle Dokumentation über die `File` Klasse](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Oracle Tutorial zu Dateien und Verzeichnissen](https://docs.oracle.com/javase/tutorial/essential/io/file.html)