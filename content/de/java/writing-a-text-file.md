---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien zu schreiben ermöglicht es, Daten dauerhaft zu speichern. Programmierer nutzen diese Funktion, um Daten zwischenzuspeichern, zu loggen oder Einstellungen zu exportieren.

## How to:
Java bietet mehrere Wege, um Textdateien zu schreiben. Hier ist ein schnelles Beispiel mit `Files` und `Path` aus dem `java.nio` Package:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class TextFileWriter {
    public static void main(String[] args) {
        String text = "Hallo, das ist ein Test!";
        Path path = Path.of("test.txt");

        try {
            Files.writeString(path, text, StandardOpenOption.CREATE);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Beispiel-Output in `test.txt`:
```
Hallo, das ist ein Test!
```

## Deep Dive
Bevor `java.nio` eingeführt wurde, schrieben Programmierer Textdateien hauptsächlich mit `FileWriter` und `BufferedWriter`. Alternativen wie `PrintWriter` oder Bibliotheken von Drittanbietern wie Apache Commons IO bieten verschiedene Funktionalitäten. Die Implementierungsdetails unterscheiden sich in Effizienz, Einfachheit und Flexibilität – `java.nio` bietet z.B. atomare Operationen und bessere Performance bei großen Dateien.

## See Also
- [File I/O in Java with `java.nio`](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Vergleich von IO und NIO in Java](https://www.baeldung.com/java-io-vs-nio)
- [Java Dokumentation für `java.io`](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
