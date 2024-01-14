---
title:    "Java: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist eine wichtige Aufgabe in der Java-Programmierung. Es ermöglicht Ihnen, sicherzustellen, dass Ihr Code mit existierenden Verzeichnissen arbeitet und potenzielle Fehler zu vermeiden.

## Wie

Zur Überprüfung der Existenz eines Verzeichnisses in Java können Sie die Methode `exists()` der Klasse `File` verwenden. Hier ist ein Beispielcode, der ein Verzeichnis auf dem Desktop mit dem Namen "test" überprüft:

```Java
File dir = new File(System.getProperty("user.home") + "/Desktop/test");
if(dir.exists()){
  System.out.println("Das Verzeichnis existiert.");
} else {
  System.out.println("Das Verzeichnis existiert nicht.");
}
```

Wenn das Verzeichnis existiert, wird die Ausgabe "Das Verzeichnis existiert." sein, andernfalls wird "Das Verzeichnis existiert nicht." ausgegeben. Sie können auch die Methode `isDirectory()` verwenden, um sicherzustellen, dass das geprüfte Objekt tatsächlich ein Verzeichnis ist.

## Deep Dive

Um tiefer in das Thema einzutauchen, betrachten wir eine weitere Möglichkeit, die Existenz eines Verzeichnisses zu überprüfen. Statt die Methode `exists()` zu verwenden, können wir auch die `list()` oder `listFiles()` Methode verwenden, um eine Liste der Dateien und Verzeichnisse in einem bestimmten Pfad zu erhalten. Wenn das Verzeichnis nicht existiert, wird eine `NullPointerException` ausgelöst, was uns sagt, dass das Verzeichnis nicht vorhanden ist.

Eine andere Sache, die es zu beachten gibt, ist, dass die `exists()` und `isDirectory()` Methoden keine Berechtigungsprüfung durchführen. Sie überprüfen lediglich, ob ein Objekt mit dem angegebenen Pfad vorhanden ist. Um sicherzustellen, dass wir auch Berechtigungen haben, um auf das Verzeichnis zuzugreifen, können wir die `canRead()` und `canWrite()` Methoden verwenden.

## Siehe auch

- [Oracle Java Dokumentation: File Klasse](https://docs.oracle.com/javase/10/docs/api/java/io/File.html)
- [Java Tutorials: Working with Directories](https://docs.oracle.com/javase/tutorial/essential/io/dirs.html)
- [Baeldung Tutorial: Check if File or Directory exists](https://www.baeldung.com/java-check-file-exists)