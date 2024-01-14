---
title:                "Java: Lesen einer Textdatei"
simple_title:         "Lesen einer Textdatei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Lesen von Textdateien ist ein häufiger Vorgang in der Programmierung. Es ermöglicht uns, Textdaten zu verarbeiten und zu analysieren, was in vielen Anwendungsfällen sehr nützlich ist. In diesem Blog-Beitrag werden wir uns damit befassen, wie wir Textdateien in Java lesen können und warum es wichtig ist, diese Fähigkeit zu beherrschen.

## Wie
Um eine Textdatei in Java zu lesen, müssen wir vier Schritte ausführen:

1. Wir müssen die Klasse `File` aus dem Package `java.io` importieren.
2. Dann müssen wir eine Instanz der `File` Klasse erstellen, indem wir den Dateipfad als Argument an den Konstruktor übergeben.
3. Als Nächstes müssen wir eine Instanz der `Scanner` Klasse aus dem Package `java.util` erstellen und die `File` Instanz als Argument an den Konstruktor übergeben.
4. Schließlich können wir mit der `Scanner` Instanz die `readLine()` Methode verwenden, um die Textdatei Zeile für Zeile zu lesen.

Schauen wir uns nun ein Beispiel an:

```Java
import java.io.File;
import java.util.Scanner;

public class TextFileReader {

    public static void main(String[] args) {
    
        // Erstellt eine Instanz der File Klasse
        File file = new File("textdatei.txt");
        
        try {
            // Erstellt eine Instanz der Scanner Klasse
            Scanner scanner = new Scanner(file);
            
            // Liest die Textdatei Zeile für Zeile und gibt sie aus
            while (scanner.hasNextLine()) {
                System.out.println(scanner.nextLine());
            }
            
            // Schließt den Scanner
            scanner.close();
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    
    }

}
```

Wenn wir diese Klasse ausführen, sehen wir die Ausgabe unserer Textdatei, die Zeile für Zeile gelesen wird:

```
Dies ist Zeile 1
Dies ist Zeile 2
Dies ist Zeile 3
```

## Deep Dive
Um tiefer in das Lesen von Textdateien in Java einzutauchen, gibt es einige wichtige Konzepte, die wir verstehen müssen. Erstens müssen wir sicherstellen, dass wir den richtigen Dateipfad angeben, wenn wir die `File` Klasse instanziieren. Andernfalls wird eine `FileNotFoundException` geworfen. Zweitens gibt es einige Methoden in der `Scanner` Klasse, die uns helfen können, die Textdatei zu analysieren, wie z.B. die `hasNextInt()` oder `hasNextDouble()` Methode, um zu überprüfen, ob der nächste Wert im Text eine ganze Zahl oder eine Fließkommazahl ist.

Es ist auch wichtig zu beachten, dass beim Lesen von Textdateien in Java Zeilenumbrüche als Trennzeichen verwendet werden. Dies bedeutet, dass sowohl `\n` als auch `\r\n` als Zeilenende interpretiert werden.

Es gibt noch viele weitere Nuancen beim Lesen von Textdateien in Java, aber mit den grundlegenden Konzepten und dem Wissen um die richtige Verwendung der `File` und `Scanner` Klassen sollten Sie in der Lage sein, die meisten Anforderungen an das Lesen von Textdateien zu erfüllen.

## Siehe auch
- [Java Dokumentation: File Klasse](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Dokumentation: Scanner Klasse](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Tutorial: Datei einlesen in Java](https://www.tutorialspoint.com/java/io/java_io_file.htm)