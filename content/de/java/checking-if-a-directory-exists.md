---
title:                "Java: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

In diesem Blogbeitrag dreht sich alles um die Überprüfung, ob ein Verzeichnis in Java existiert. Die Überprüfung der Existenz eines Verzeichnisses ist ein wichtiger Schritt bei der Handhabung von Dateien in Java. Sie dient dazu, Fehler zu vermeiden und eine reibungslose Ausführung des Codes zu gewährleisten.

# So geht's

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Methode `exists()` der Klasse `File` verwenden. Hier ist ein Beispielcode, der ein Verzeichnis mit dem Namen `testDirectory` erstellt und dann überprüft, ob es existiert:

```Java
// Erstelle ein Verzeichnis mit dem Namen "testDirectory"
File directory = new File("testDirectory");

// Überprüfe, ob das Verzeichnis existiert
if (directory.exists()) {
    System.out.println("Das Verzeichnis existiert.");
} else {
    System.out.println("Das Verzeichnis existiert nicht.");
}
```

Wenn das Verzeichnis bereits vorhanden ist, wird die Ausgabe "Das Verzeichnis existiert." erscheinen. Ansonsten wird "Das Verzeichnis existiert nicht." angezeigt. Wir können auch weitere Methoden wie `isDirectory()` und `isFile()` verwenden, um zu überprüfen, ob es sich tatsächlich um ein Verzeichnis handelt und nicht um eine Datei.

# Eintauchen

Die Methode `exists()` verwendet das Betriebssystem, um zu überprüfen, ob das Verzeichnis existiert oder nicht. Wenn das Verzeichnis nicht vorhanden ist, gibt die Methode `exists()` `false` zurück und alle weiteren Überprüfungen werden übersprungen. Es ist wichtig zu beachten, dass diese Methode keine Ausnahmen wirft und daher möglicherweise nicht immer eine zuverlässige Methode ist, um die Existenz eines Verzeichnisses zu überprüfen.

Es gibt auch eine andere Methode namens `getAbsolutePath()`, mit der wir den absoluten Pfad eines Verzeichnisses oder einer Datei erhalten können. Dies kann hilfreich sein, um sicherzustellen, dass das Verzeichnis, das wir überprüfen möchten, tatsächlich an der angegebenen Stelle existiert.

# Siehe auch

- [Java File Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java File Class Tutorial](https://www.baeldung.com/java-file)
- [Überprüfen der Existenz eines Verzeichnisses in Java](https://stackoverflow.com/questions/1791285/checking-if-a-directory-exists-in-java)