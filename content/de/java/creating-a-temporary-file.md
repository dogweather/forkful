---
title:                "Erstellen einer temporären Datei"
html_title:           "Java: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ist ein häufiges Konzept in der Programmierung, das es ermöglicht, temporäre Daten oder Zwischenergebnisse zu speichern. Programmierer nutzen dies, um z.B. Daten zwischen Prozessen oder Programmen auszutauschen.

## Wie geht's?

In Java gibt es die Klasse "File", die verschiedene Methoden zum Erstellen von temporären Dateien bereitstellt. Ein Beispiel dafür wäre:

```Java
// Importieren der benötigten Bibliotheken
import java.io.*;

// Erstellen einer temporären Datei
File tempFile = File.createTempFile(prefix, suffix);

// Schreiben von Daten in die temporäre Datei
FileWriter tempWriter = new FileWriter(tempFile);
tempWriter.write("Dies ist ein Beispieltext.");
tempWriter.close();

// Auslesen der Daten aus der temporären Datei
FileReader tempReader = new FileReader(tempFile);
BufferedReader br = new BufferedReader(tempReader);
String line = br.readLine();
System.out.println(line);
```

Dieses Beispiel erstellt eine temporäre Datei mit einem eindeutigen Namen (durch den optionalen Parameter "prefix" und "suffix") und schreibt dann einen Beispieltext in die Datei. Anschließend wird der Inhalt der Datei ausgelesen und in der Konsole ausgegeben.

## Tiefer eintauchen

Das Konzept der temporären Dateien ist nicht neu und wird schon lange in der Programmierung verwendet. Früher mussten Programmierer manuell eine Datei mit einem eindeutigen Namen erstellen und die Daten darin speichern. Durch die Einführung der "File" Klasse in Java wurde dieser Prozess vereinfacht und standardisiert.

Für die Erstellung von temporären Dateien gibt es auch alternative Ansätze wie z.B. die Verwendung der "java.nio" Pakete oder die Nutzung von "RandomAccessFile". Diese bieten möglicherweise noch mehr Flexibilität oder bessere Performance, je nach Anwendungsfall.

Die Implementation von temporären Dateien erfolgt in der Regel durch das Erstellen einer Datei im Verzeichnis "tmp" des Betriebssystems. Diese Dateien werden in der Regel automatisch gelöscht, wenn das Programm beendet wird oder können auch manuell gelöscht werden, falls notwendig.

## Siehe auch

- [Oracle Java Dokumentation zur File Klasse](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Beispielimplementierung von Java Temporary Files auf GitHub](https://github.com/javadev/temporary-files)