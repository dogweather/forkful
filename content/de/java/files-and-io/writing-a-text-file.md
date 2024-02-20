---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:05.563840-07:00
description: "Das Schreiben einer Textdatei in Java bedeutet, die F\xE4higkeiten der\
  \ Sprache zu nutzen, um Inhalte in Dateien auf dem Dateisystem zu erstellen und\
  \ zu\u2026"
lastmod: 2024-02-19 22:05:12.706534
model: gpt-4-0125-preview
summary: "Das Schreiben einer Textdatei in Java bedeutet, die F\xE4higkeiten der Sprache\
  \ zu nutzen, um Inhalte in Dateien auf dem Dateisystem zu erstellen und zu\u2026"
title: Eine Textdatei schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei in Java bedeutet, die Fähigkeiten der Sprache zu nutzen, um Inhalte in Dateien auf dem Dateisystem zu erstellen und zu schreiben. Programmierer tun dies aus verschiedenen Gründen, wie zum Beispiel Logging, Exportieren von Daten oder Speichern des Anwendungszustands zur späteren Abrufung.

## Wie geht das:

### Verwendung von `java.nio.file` (Standardbibliothek)

Das New I/O (NIO)-Paket von Java (`java.nio.file`) bietet einen vielseitigeren Ansatz im Umgang mit Dateien. Hier ist eine einfache Möglichkeit, in eine Datei zu schreiben, mit `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Zeile 1", "Zeile 2", "Zeile 3");
        try {
            Files.write(Paths.get("beispiel.txt"), lines);
            System.out.println("Datei erfolgreich geschrieben!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Ausgabe:

```
Datei erfolgreich geschrieben!
```

### Verwendung von `java.io` (Standardbibliothek)

Für einen traditionelleren Ansatz ist `java.io.FileWriter` eine gute Wahl, um Textdateien einfach zu schreiben:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("beispiel.txt")) {
            writer.write("Hallo, Welt!\n");
            writer.append("Das ist eine weitere Zeile.");
            System.out.println("Datei erfolgreich geschrieben!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Ausgabe:

```
Datei erfolgreich geschrieben!
```

### Verwendung von Apache Commons IO

Die Bibliothek Apache Commons IO vereinfacht viele Operationen, einschließlich des Schreibens von Dateien. So schreiben Sie in eine Datei mit `FileUtils.writeStringToFile()`:

Fügen Sie zuerst die Abhängigkeit zu Ihrem Projekt hinzu. Wenn Sie Maven verwenden, fügen Sie hinzu:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Prüfen Sie die neueste Version -->
</dependency>
```

Verwenden Sie dann den folgenden Code, um Text in eine Datei zu schreiben:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("beispiel.txt"), "Dies ist Text, der mit Commons IO geschrieben wurde.", "UTF-8");
            System.out.println("Datei erfolgreich geschrieben!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Ausgabe:

```
Datei erfolgreich geschrieben!
```
