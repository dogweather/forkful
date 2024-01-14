---
title:    "Java: Erstellen einer temporären Datei"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man im Java-Programmierbereich temporäre Dateien erstellen könnte. Die häufigsten Gründe sind Speichern von temporären Daten, Erstellen von temporären Backups und Testen von Dateifunktionen.

## Wie man eine temporäre Datei erstellt
Das Erstellen einer temporären Datei in Java ist einfach. Zunächst importieren wir die Klasse `java.io.File`, die im Package `java.io` zu finden ist. Dann definieren wir den Dateipfad und den Dateinamen, den wir für unsere temporäre Datei verwenden möchten.

```Java
import java.io.File;

// Define file path and name
String filePath = "C:\\temp";
String fileName = "temp-file.txt";
```

Als nächstes erstellen wir eine Instanz der Klasse `File` und übergeben die Pfade und den Dateinamen als Parameter.

```Java
// Create File instance
File tempFile = new File(filePath, fileName);
```

Um die temporäre Datei tatsächlich zu erstellen, rufen wir die Methode `createNewFile()` auf, die eine `IOException` werfen kann. Daher müssen wir diese in einem `try-catch`-Block behandeln.

```Java
try {
    // Create temp file
    tempFile.createNewFile();
    System.out.println("Temporary file created: " + tempFile.getAbsolutePath());
} catch (IOException e) {
    e.printStackTrace();
}
```

Wenn die Datei erfolgreich erstellt wurde, erhalten wir als Ausgabe den absoluten Pfad zu unserer temporären Datei.

## Tiefere Einblicke
Das Erstellen von temporären Dateien kann manchmal mehr als nur das einfache Erstellen einer Datei erfordern. Hier sind einige weitere Dinge, die man beachten sollte:

- Es ist wichtig sicherzustellen, dass die temporäre Datei nicht bereits existiert, bevor sie erstellt wird. Ansonsten kann dies zu Konflikten oder Fehlermeldungen führen.
- Nach der Verwendung sollten temporäre Dateien auch wieder gelöscht werden, um Speicherplatz zu sparen.
- Es gibt auch die Möglichkeit, eine temporäre Datei in einem speziellen Ordner zu erstellen, den Java automatisch verwaltet. Dazu muss man lediglich die Methode `createTempFile()` verwenden und den Ordner festlegen.

## Siehe auch
- [Java Dokumentation: java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Tutorial: Verwenden von temporären Dateien in Java](https://www.baeldung.com/java-temporary-file)