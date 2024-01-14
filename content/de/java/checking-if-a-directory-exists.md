---
title:                "Java: Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Warum sollten wir uns überhaupt Gedanken darüber machen, ob ein Verzeichnis existiert? Nun, es gibt verschiedene Szenarien, in denen es wichtig sein könnte zu überprüfen, ob ein Verzeichnis vorhanden ist. Zum Beispiel wenn wir ein Programm haben, das Dateien in einem bestimmten Verzeichnis speichert oder liest, möchten wir sicherstellen, dass dieses Verzeichnis tatsächlich existiert, bevor wir versuchen, auf Dateien zuzugreifen.

## Wie geht das?
In Java gibt es verschiedene Möglichkeiten, um zu überprüfen, ob ein Verzeichnis existiert. Die einfachste Methode besteht darin, die Methode `exists()` auf der `File`-Klasse aufzurufen und ihr den Pfad zum Verzeichnis zu übergeben. Hier ist ein Beispiel:

```Java
File directory = new File("Pfad/zum/Verzeichnis");

if (directory.exists()) {
  System.out.println("Das Verzeichnis existiert!");
} else {
  System.out.println("Das Verzeichnis existiert nicht.");
}
```

Je nachdem, ob das Verzeichnis tatsächlich vorhanden ist oder nicht, wird eine entsprechende Meldung ausgegeben.

## Tiefergehende Informationen
Wenn wir genauer verstehen möchten, wie die Methode `exists()` funktioniert, können wir uns den Quellcode der `File`-Klasse ansehen. Diese Methode verwendet intern die `AccessController`-Klasse, um die Rechte auf dem Dateisystem zu überprüfen. Sie gibt `true` zurück, wenn der Zugriff auf das Verzeichnis gewährt werden kann, oder `false`, wenn dies nicht der Fall ist.

Außerdem können wir auch die Methode `canRead()` oder `canWrite()` verwenden, um zu überprüfen, ob wir auf das Verzeichnis lesend oder schreibend zugreifen können.

## Siehe auch
- [Java Dokumentation - File Klasse](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Dokumentation - AccessController Klasse](https://docs.oracle.com/javase/8/docs/api/java/security/AccessController.html)
- [Java Dokumentation - Verzeichniszugriff](https://docs.oracle.com/javase/tutorial/essential/io/dirs.html)