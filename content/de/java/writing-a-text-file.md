---
title:                "Eine Textdatei schreiben"
html_title:           "Java: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren in Java gibt es viele Situationen, in denen das Schreiben einer Textdatei erforderlich ist. Zum Beispiel kann man damit Daten speichern, die später wieder abgerufen und verarbeitet werden sollen.

## So geht's

Um eine Textdatei in Java zu schreiben, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung der Klasse "FileWriter", die ermöglicht, einen Text in die Datei zu schreiben.

```Java
try {
    // Datei erstellen oder öffnen
    FileWriter fw = new FileWriter("beispiel.txt");

    // Text in Datei schreiben
    fw.write("Dies ist ein Beispieltext.");

    // FileWriter schließen
    fw.close();
} catch (IOException e) {
    // Fehlerbehandlung
    System.out.println("Beim Schreiben der Datei ist ein Fehler aufgetreten.");
}
```

Dieses Beispiel zeigt, wie man mit dem FileWriter eine Textdatei erstellen oder öffnen und Text in die Datei schreiben kann. Dabei ist es wichtig, dass man den FileWriter am Ende immer schließt, um mögliche Fehler zu vermeiden.

## Tiefere Einblicke

Neben dem FileWriter gibt es noch weitere Möglichkeiten, um in Java Textdateien zu schreiben. Zum Beispiel kann man auch die Klasse "BufferedWriter" verwenden, um die Geschwindigkeit der Textverarbeitung zu erhöhen. Dabei wird der FileWriter in den BufferedWriter "eingewickelt", um zusätzliche Funktionen hinzuzufügen.

```Java
try {
    // Datei erstellen oder öffnen
    FileWriter fw = new FileWriter("beispiel.txt");

    // BufferedWriter erstellen
    BufferedWriter bw = new BufferedWriter(fw);

    // Text in Datei schreiben
    bw.write("Dies ist ein Beispieltext.");

    // BufferedWriter schließen
    bw.close();
} catch (IOException e) {
    // Fehlerbehandlung
    System.out.println("Beim Schreiben der Datei ist ein Fehler aufgetreten.");
}
```

Außerdem ist es auch möglich, Textdateien mithilfe von Streams zu erstellen und zu schreiben. Dabei kann man auch den Zeichensatz, in dem die Datei geschrieben werden soll, festlegen.

## Siehe auch

- [Java-Dateien und Streams](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Java FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Java BufferedWriter](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html)