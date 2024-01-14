---
title:                "C#: Das Lesen einer Textdatei"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von einer Textdatei ist eine grundlegende Fähigkeit für alle Programmierer. Es ermöglicht uns, Daten aus externen Quellen in unsere Programme zu integrieren und zu verarbeiten. In dieser Blog-Post werden wir uns genauer anschauen, wie wir eine Textdatei in C# lesen können und warum es eine wichtige Fähigkeit ist, die jeder Programmierer wissen sollte.

## Wie man eine Textdatei in C# liest

Zuerst müssen wir eine Verbindung zur Textdatei herstellen. Dazu können wir die `FileStream` Klasse verwenden und den Dateipfad als Argument übergeben.

```C#
FileStream fileStream = new FileStream("C:/Users/MeineDatei.txt", FileMode.Open);
```

Als nächstes müssen wir den `StreamReader` initialisieren, um den Text in der Datei zu lesen.

```C#
StreamReader reader = new StreamReader(fileStream);
```

Wir können nun die Methode `ReadLine()` verwenden, um den Inhalt der Datei Zeile für Zeile zu lesen und in einer Variablen zu speichern.

```C#
string line = reader.ReadLine();
```

Dieser Vorgang kann in einer Schleife durchgeführt werden, um alle Zeilen in der Textdatei zu lesen.

```C#
while(!reader.EndOfStream)
{
    string line = reader.ReadLine();
    Console.WriteLine(line);
}
```

Die Ausgabe hier ist eine einfache Konsolenausgabe, aber je nach Bedarf können wir den gelesenen Text auch in einem Array oder einer anderen Datenstruktur speichern.

## Tiefergehende Informationen über das Lesen von Textdateien

Beim Lesen von Textdateien gibt es einige Dinge zu beachten. Zum einen ist es wichtig, die Datei nach dem Lesen immer wieder zu schließen. Dazu können wir die Methode `Close()` aufrufen oder den `StreamReader` in einem `using`-Block verwenden, der automatisch die Datei schließt, wenn wir aus dem Block herausgehen.

```C#
using (StreamReader reader = new StreamReader(fileStream))
{
    // Code zum Lesen der Datei
}
```

Eine weitere wichtige Überlegung ist die Behandlung von Fehlern. Beim Lesen einer Datei kann es zu Fehlermeldungen kommen, wenn die Datei nicht gefunden wird oder nicht lesbar ist. Daher ist es wichtig, entsprechende Fehlerbehandlungen in unserem Code zu implementieren.

## Siehe auch

- [C# Dokumentation über das Lesen von Textdateien](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file)
- [Eine Einführung in die Grundlagen der Dateiverarbeitung in C#](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
- [Einen CSV-Parser in C# schreiben](https://www.codeproject.com/Articles/415732/Reading-and-Writing-CSV-Files-in-Csharp)

Durch das Lesen von Textdateien in C# können wir unsere Programme um zusätzliche Funktionalitäten erweitern und Daten effektiv verarbeiten. Es ist eine wichtige Fähigkeit, die uns als Programmierer dabei helfen kann, unsere Projekte zu verbessern und zu skalieren. Mit den richtigen Kenntnissen und Werkzeugen können wir problemlos Textdateien in unsere C#-Anwendungen einbinden und nutzen.