---
date: 2024-01-20 17:54:38.810478-07:00
description: "Das Lesen einer Textdatei bedeutet, ihren Inhalt in dein Programm zu\
  \ laden. Programmierer tun dies, um Daten zu verarbeiten, Konfigurationen zu laden\
  \ oder\u2026"
lastmod: '2024-03-13T22:44:53.779768-06:00'
model: gpt-4-1106-preview
summary: "Das Lesen einer Textdatei bedeutet, ihren Inhalt in dein Programm zu laden.\
  \ Programmierer tun dies, um Daten zu verarbeiten, Konfigurationen zu laden oder\u2026"
title: Textdatei einlesen
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei bedeutet, ihren Inhalt in dein Programm zu laden. Programmierer tun dies, um Daten zu verarbeiten, Konfigurationen zu laden oder Informationen zu speichern.

## How to:
Java bietet verschiedene Wege, um Textdateien zu lesen. Hier ist ein einfacher Ansatz mit `java.nio.file.Files`:

```java
import java.nio.file.Paths;
import java.nio.file.Files;
import java.io.IOException;
import java.util.List;

public class TextFileReader {
    public static void main(String[] args) {
        String filePath = "beispiel.txt"; // Der Pfad zur Datei

        try {
            List<String> lines = Files.readAllLines(Paths.get(filePath));
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Angenommen `beispiel.txt` enthält:
```
Hallo Welt!
Das ist eine Textdatei.
```

Die Ausgabe wird sein:
```
Hallo Welt!
Das ist eine Textdatei.
```

## Deep Dive
Das Lesen von Dateien in Java hat sich entwickelt. In frühen Java-Versionen war `java.io.BufferedReader` üblich. `java.nio.file.Files` ist seit Java 7 verfügbar und nutzt einen effizienteren Ansatz.

Alternativen:

- `Scanner` für einfaches Parsen von Primitiven und String.
- `BufferedReader` für größere Dateien, wenn wir nur Zeile für Zeile lesen möchten.

Implementierungsdetails:

- `Files.readAllLines` liest alle Zeilen auf einmal. Geeignet für kleine bis mittelgroße Dateien.
- Achte auf `IOException`, die Fehler beim Lesen signalisiert.
- Für große Dateien solltest du einen Stream verwenden, um den Speicherbedarf gering zu halten:

```java
Files.lines(Paths.get(filePath)).forEach(System.out::println);
```

## See Also
Hier sind nützliche Links, um mehr zu erfahren:

- [Oracle's official Java documentation on File I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Java NIO file package summary](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- [Baeldung’s Guide on reading a file into a String](https://www.baeldung.com/reading-file-in-java)
