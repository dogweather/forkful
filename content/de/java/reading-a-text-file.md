---
title:                "Das Lesen einer Textdatei"
html_title:           "Java: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum es wichtig ist, eine Textdatei mit Java zu lesen. Nun, es gibt viele Gründe dafür! Zum Beispiel kann das Lesen von Textdateien von entscheidender Bedeutung sein, wenn du mit großen Mengen von Daten arbeitest oder wenn du Daten von einem externen Quellsystem in dein Programm importieren möchtest. Durch das Lesen von Textdateien kannst du auch den Inhalt einer Datei analysieren und darauf basierend deine Entscheidungen im Code treffen.

## Wie es funktioniert

Um eine Textdatei in Java zu lesen, müssen wir zuerst die Klasse "File" importieren und eine neue Instanz dieser Klasse erstellen, die die Datei repräsentiert, die wir lesen möchten. Dann müssen wir einen "FileReader" erstellen, der die Datei in ein lesbares Format umwandelt. Als nächstes verwenden wir einen "BufferedReader", um die Datei zeilenweise zu lesen. Hier ist ein Beispielcode:

```Java
import java.io.*;

public class TextFileReader {

    public static void main(String[] args) {
        // Datei "test.txt" öffnen
        File file = new File("test.txt");
  
        // FileReader-Objekt erstellen
        FileReader fr = new FileReader(file);

        // BufferedReader-Objekt erstellen
        BufferedReader br = new BufferedReader(fr);
  
        // Zeile für Zeile lesen und ausgeben
        String line;
        while ((line = br.readLine()) != null) {
            System.out.println(line);
        }
    }
}
```

Die obige Codebeispiel verwendet die Methode ".readLine()", um jede Zeile aus der Datei auszulesen und sie dann mit der Methode ".println()" auf der Konsole auszugeben. Wenn du den Code ausführst, erhältst du die Ausgabedaten wie folgt:

```
Diese Datei wird mit Java gelesen.
Hier ist ein Beispiel mit einigen Zeilen.
Wir können diese Daten jetzt weiterverarbeiten.
```

In der obigen Ausgabe siehst du, dass jede Zeile aus der Textdatei einzeln ausgegeben wird.

## Tiefergehende Einblicke

Das Lesen von Textdateien in Java kann jedoch etwas komplexer sein, wenn du bestimmte Zeichen oder Muster in der Datei finden und verarbeiten musst. Hier kommt die Klasse "Scanner" ins Spiel. Der "Scanner" wird verwendet, um Textdateien effizient zu lesen und bestimmte Muster abzugleichen. Hier ist ein Beispielcode, der die Wörter "Java" in der Datei sucht und zählt, wie oft es vorkommt:

```Java
import java.io.*;
import java.util.Scanner;

public class SearchText {

    public static void main(String[] args) {
        // Datei "test.txt" öffnen
        File file = new File("test.txt");
  
        // Scanner-Objekt erstellen
        Scanner scanner = new Scanner(file);
  
        // Zähler für "Java" definieren
        int count = 0;

        // Textdatei zeilenweise durchsuchen
        while (scanner.hasNextLine()) {
            // Zeile auslesen
            String line = scanner.nextLine();
            // Muster "Java" in jeder Zeile suchen und Zähler erhöhen
            if (line.contains("Java")) {
                count++;
            }
        }
  
        // Anzahl der gefundenen "Java"-Wörter ausgeben
        System.out.println("Das Wort \"Java\" kommt " + count + " mal vor.");
    }
}
```

Wenn du die obige Beispielcode ausführst, solltest du eine Ausgabe ähnlich dieser erhalten:

```
Das Wort "Java" kommt 1 mal vor.
```

Durch die Verwendung des "Scanner" können wir also effektiv bestimmte Muster in der Textdatei finden und dann entsprechend darauf reagieren.

## Siehe auch

- Java File-Klasse: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- Java Scanner-Klasse: https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html