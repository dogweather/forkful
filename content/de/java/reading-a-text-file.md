---
title:    "Java: Eine Textdatei lesen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit in der Java-Programmierung. Mit der richtigen Anleitung können Sie so Zugriff auf große Mengen an Textdaten erhalten und diese weiterverarbeiten.

## Wie Geht's

1. Öffnen Sie die Textdatei mit der `FileReader` Klasse und geben Sie den Pfad der Datei als Argument ein.
2. Verwenden Sie einen `BufferedReader`, um die Datei Zeile für Zeile zu lesen.
3. In einer Schleife können Sie auf jede gelesene Zeile mit der `readLine()` Methode zugreifen und diese weiterverarbeiten.
4. Vergessen Sie nicht, die Datei auf das Ende der Verarbeitung mit `close()` zu schließen.

In Java Code sieht das so aus:

```Java
// Öffnen der Textdatei
FileReader fileReader = new FileReader("pfad/zur/datei.txt"); 

// Erstellen des BufferedReader
BufferedReader bufferedReader = new BufferedReader(fileReader);

// Lesen und Ausgeben der Datei Zeile für Zeile
String line = bufferedReader.readLine(); // liest eine Zeile
while (line != null) { // solange noch Zeilen vorhanden sind
    System.out.println(line); // gibt die Zeile aus
    line = bufferedReader.readLine(); // liest die nächste Zeile
}

// Schließen der Datei
bufferedReader.close();
```

Das könnte zum Beispiel folgenden Output erzeugen:

```
Dies ist die erste Zeile.
Dies ist die zweite Zeile.
Dies ist die dritte Zeile.
```

## Tiefer Einblick

Es gibt noch einige weitere Möglichkeiten, um Textdateien in Java zu lesen. Zum Beispiel können Sie die `Scanner` Klasse verwenden, um die Datei nach bestimmten Mustern zu durchsuchen. Auch das Lesen von binären Dateien mit der `FileInputStream` und `DataInputStream` Klasse ist möglich. Diese Techniken erfordern jedoch etwas fortgeschrittenere Kenntnisse in der Programmierung.

## Siehe Auch

- [Java FileReader Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Java BufferedReader Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)
- [Java Scanner Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)