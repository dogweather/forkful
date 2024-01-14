---
title:    "Java: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, kann in verschiedenen Szenarien nützlich sein, insbesondere in der Programmierung. Es ermöglicht uns festzustellen, ob ein bestimmter Ordner auf unserem System vorhanden ist, bevor wir versuchen, darauf zuzugreifen oder ihn zu bearbeiten. Dies kann Fehler vermeiden und die Zuverlässigkeit unseres Codes erhöhen.

## Wie man es macht

Um zu überprüfen, ob ein Verzeichnis in Java existiert, können wir die Methode `exists()` der Klasse `File` verwenden. Diese Methode gibt einen booleschen Wert zurück, der angibt, ob das Verzeichnis existiert oder nicht. Nehmen wir zum Beispiel an, wir wollen überprüfen, ob das Verzeichnis "Dokumente" auf unserem Desktop existiert. Dazu könnten wir folgenden Code verwenden:

```Java
File file = new File(System.getProperty("user.home") + "/Desktop/Dokumente");
if (file.exists()) {
  System.out.println("Das Verzeichnis existiert.");
} else {
  System.out.println("Das Verzeichnis existiert nicht.");
}
```
Die Variable `file` wird zunächst mit dem Pfad des Verzeichnisses initialisiert, indem wir die Systemeigenschaft `user.home` und den Pfad zum Desktop kombinieren. Dann überprüfen wir mit der `exists()` Methode, ob das Verzeichnis existiert und geben je nach Ergebnis eine entsprechende Meldung aus.

## Tiefer gehend

Die Methode `exists()` überprüft lediglich, ob ein Eintrag mit dem angegebenen Pfad existiert, unabhängig davon, ob es sich um ein Verzeichnis oder eine Datei handelt. Wenn wir sicherstellen wollen, dass es sich um ein Verzeichnis handelt, können wir die Methode `isDirectory()` nutzen. Diese gibt ebenfalls einen booleschen Wert zurück und überprüft, ob der Eintrag ein Verzeichnis ist. Wir können dies in unserem Beispiel folgendermaßen implementieren:

```Java
if (file.exists() && file.isDirectory()) {
    System.out.println("Das Verzeichnis existiert.");
} else {
    System.out.println("Das Verzeichnis existiert nicht oder es handelt sich nicht um ein Verzeichnis.");
}
```

## Siehe auch

- [Java File API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Oracle Java Tutorials: Checking for Files](https://docs.oracle.com/javase/tutorial/essential/io/check.html)
- [Java - Überprüfe, ob ein Ordner vorhanden ist](https://www.tutorialspoint.com/javaexamples/file_checkdir.htm)