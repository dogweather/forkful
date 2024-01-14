---
title:    "Java: Eine Textdatei schreiben"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Schreiben eines Textdatei ist eine essentielle Fähigkeit für jeden Java-Programmierer. Es ermöglicht das Speichern und Zugreifen von Daten in einer strukturierten und einfach zu verwendenden Form.

## Wie

Ein Beispielcode für das Schreiben einer Textdatei in Java:

```Java
import java.io.FileWriter;
import java.io.IOException;
 
public class TextWriter {
    public static void main(String[] args) {
        // Pfad und Name der Textdatei
        String fileName = "beispiel.txt";
        
        try {
            // Textdatei erstellen
            FileWriter writer = new FileWriter(fileName);
             
            // Text in die Datei schreiben
            writer.write("Hallo, das ist ein Beispieltext.");
             
            // Writer schließen
            writer.close();
        } catch (IOException e) {
            System.out.println("Fehler beim Schreiben der Datei " + fileName + ".");
            e.printStackTrace();
        }
    }
}
```

Die oben genannte Datei wird eine Datei mit dem Namen "beispiel.txt" erstellen und den Text "Hallo, das ist ein Beispieltext." in die Datei schreiben.

## Deep Dive

Es gibt verschiedene Möglichkeiten, eine Textdatei in Java zu schreiben. Eine andere Möglichkeit ist die Verwendung der Klasse PrintWriter. Eine andere wichtige Sache zu beachten ist, dass der Pfad zum Speichern der Textdatei im Code angegeben werden muss.

## Siehe auch

- [Java Dokumentation: FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Java Dokumentation: PrintWriter](https://docs.oracle.com/javase/7/docs/api/java/io/PrintWriter.html)
- [Java Dateisystem: Working with Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)