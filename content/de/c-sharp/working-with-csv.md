---
title:                "C#: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# Warum

Das Arbeiten mit CSV-Dateien ist ein wichtiger Bestandteil der Datenverarbeitung in der Entwicklungsbranche. Es ermöglicht das einfache Speichern und Übertragen von Daten in einem tabellarischen Format. Mit C# können CSV-Dateien einfach erstellt und bearbeitet werden, was zu einer effizienten und effektiven Arbeitsweise führt.

# Wie geht das?

Um CSV-Dateien in C# zu verarbeiten, müssen Sie zunächst die <code>System.IO</code> Namespace importieren. Dann können Sie die <code>StreamReader</code> und <code>StreamWriter</code> Klassen verwenden, um die lesende und schreibende Funktionalität zu implementieren.

```C#
using System.IO;
```

Als nächstes müssen Sie eine Instanz der <code>StreamReader</code> Klasse erstellen, um die CSV-Datei zu lesen. Dies kann mit dem <code>File.OpenText()</code> Befehl erfolgen, der den Dateipfad als Argument annimmt.

```C#
StreamReader reader = File.OpenText("pfad/zu/der/csv-datei.csv");
```

Um die Daten aus der CSV-Datei zu lesen, können Sie die <code>ReadLine()</code> Methode verwenden, die jede Zeile als Zeichenfolge zurückgibt.

```C#
string zeile = reader.ReadLine();
```

Nachdem Sie die Daten gelesen haben, können Sie diese in einem Array speichern, indem Sie die <code>Split()</code> Methode verwenden, die die Zeichenfolge anhand des angegebenen Trennzeichen in separate Elemente aufteilt.

```C#
string[] daten = zeile.Split(',');
```

Um eine neue CSV-Datei zu erstellen und Daten hinzuzufügen, muss zunächst eine Instanz der <code>StreamWriter</code> Klasse erstellt werden. Dann können Sie die <code>WriteLine()</code> Methode verwenden, um die Daten in das gewünschte Format zu schreiben.

```C#
StreamWriter writer = new StreamWriter("pfad/zur/neuen/csv-datei.csv");
writer.WriteLine("Name, Alter, Stadt");
writer.WriteLine("Max, 25, Berlin");
```

# Tiefergehende Informationen

Beim Verarbeiten von CSV-Dateien gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen Sie möglicherweise die verwendeten Trennzeichen anpassen, je nachdem, in welchem Format die Daten gespeichert sind. Außerdem ist es wichtig, das Encoding der Dateien zu überprüfen, um sicherzustellen, dass die Daten korrekt gelesen und geschrieben werden.

Es kann auch hilfreich sein, Bibliotheken wie die <code>CSVHelper</code> zu verwenden, die erweiterte Funktionen für das Lesen und Schreiben von CSV-Dateien bieten. Diese Bibliotheken können das Handling von Zeichenfolgen, Zahlen und Datumsformaten erleichtern.

# Siehe auch

- [Verarbeiten von CSV-Dateien in C#](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time-visual-basic)
- [CSV-Dateien mit der CSVHelper-Bibliothek verarbeiten](https://joshclose.github.io/CsvHelper/)