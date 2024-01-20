---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Lesen einer Textdatei ist der Prozess, bei dem Daten aus einer Datei auf der Festplatte in ein Programm in Arbeitsspeicher geladen werden. Programmierer tun dies, um Dateien zu analysieren, den Code zu debuggen oder um Daten für die weitere Verarbeitung zu laden.

## So funktioniert's:

Im Folgenden sehen Sie, wie Sie mit Java eine Textdatei lesen können. Dieses Beispiel verwendet die Klasse `Files` und die Methode `readAllLines()`. Beachten Sie, dass hierfür Java 8 oder höher erforderlich ist.

```Java
import java.nio.file.*;

public class TextFileReader {
    public static void main(String[] args) {
        Path filePath = Paths.get("test.txt");

        try {
            List<String> lines = Files.readAllLines(filePath);

            for (String line : lines) {
                System.out.println(line);
            }
        } catch(IOException e) {
            System.out.println("Fehler: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
```
Und dies ist die Ausgabe:

```
Hallo, das ist ein Text!
Zweite Zeile hier.
Und das ist die dritte.
```

## Mehr Details:

Textdateien wurden in den Anfängen der Computerprogrammierung eingeführt und werden auch heute noch weitgehend genutzt. Alternativen zum Einlesen einer Textdatei sind beispielsweise XML oder JSON Dateien, wobei der Vorteil dabei ist, dass diese Formate structurierte Daten ermöglichen.

Wenn Sie mit sehr großen Textdateien arbeiten, sollten Sie `BufferedReader` und `FileReader` verwenden, um Speicher zu sparen. Buffern bedeutet, dass das Programm nur einen kleinen Teil der Datei auf einmal in den Arbeitsspeicher lädt. Hier ist ein Beispiel, wie das funktioniert:

```Java
import java.io.*;

public class LargeFileReader {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("large-file.txt"));

            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
            reader.close();
        } catch(IOException e) {
            System.out.println("Fehler: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
```

## Weiterführende Links:

Um mehr über Dateiverarbeitung mit Java zu lernen, besuchen Sie die folgenden Links:

- Oracle Dokumentation: `[Arbeiten mit Java-Dateien](https://docs.oracle.com/javase/tutorial/essential/io/file.html)`
- GeeksforGeeks Tutorial: `[Einlesen einer Textdatei in Java](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)`