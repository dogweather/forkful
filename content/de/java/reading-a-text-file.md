---
title:                "Java: Eine Textdatei lesen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen einer Textdatei ist ein grundlegendes Konzept beim Programmieren in Java, das jeder lernen sollte. Textdateien sind eine häufig verwendete Methode, um Daten zu speichern und zu übertragen. Durch das Erlernen dieses Konzepts können Sie in der Lage sein, komplexe Anwendungen zu schreiben und effizient mit großen Mengen von Daten umzugehen.

# How To

Um eine Textdatei in Java zu lesen, gibt es mehrere Schritte, die befolgt werden müssen:

1. Öffnen Sie die Textdatei mit der `FileReader`-Klasse und geben Sie den Pfad zur Datei an.
2. Verwenden Sie die `BufferedReader`-Klasse, um die Datei zeilenweise zu lesen.
3. Verwenden Sie eine Schleife, um die Zeilen einzeln zu lesen und die Daten zu verarbeiten.

```Java
FileReader fileReader = new FileReader("pfad/zur/datei.txt");
BufferedReader bufferedReader = new BufferedReader(fileReader);

String line;
while ((line = bufferedReader.readLine()) != null) {
    // Verarbeiten Sie jede Zeile hier
    System.out.println(line);
}

bufferedReader.close();
```

Ausgabe:

```
Dies ist eine Zeile.
Dies ist eine andere Zeile.
Und dies ist eine dritte Zeile.
```

# Deep Dive

Ein wichtiger Aspekt beim Lesen von Textdateien ist das Handling von Fehlern. Beispielsweise kann der Pfad zur Datei möglicherweise nicht existieren oder die Datei könnte beschädigt sein. In solchen Fällen wird eine `IOException` ausgelöst, die behandelt werden muss, um zu verhindern, dass die Anwendung abstürzt.

Außerdem ist es wichtig zu beachten, dass Zeilenendungen in Textdateien je nach Betriebssystem unterschiedlich sein können. Während auf Windows-Systemen normalerweise `\r\n` als Zeilenende verwendet wird, wird auf Unix-Systemen `\n` verwendet. Um dies zu berücksichtigen, empfiehlt es sich, die `System.lineSeparator()`-Methode zu verwenden, die automatisch das richtige Zeilenende für das aktuelle System zurückgibt.

# Siehe auch

- [Java FileReader Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java BufferedReader Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Using Try-Catch Blocks in Java](https://www.geeksforgeeks.org/try-catch-block-java/) (Englisch)