---
title:                "C#: Das Schreiben einer Textdatei"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit für alle, die C# programmieren. Mit Textdateien können Sie Daten speichern, die vom Benutzer eingegeben wurden, Ergebnisse von Berechnungen ausgeben und vieles mehr. Es ist auch eine effektive Möglichkeit, Informationen zu speichern, ohne eine komplexe Datenbank einzurichten.

## Wie man es macht

Um eine Textdatei in C# zu schreiben, müssen Sie zunächst die `StreamWriter` -Klasse verwenden. Beginnen Sie mit der Erstellung einer Instanz der `StreamWriter` -Klasse und übergeben Sie als Parameter den Namen und den Speicherort der zu erstellenden Textdatei.

```C#
StreamWriter writer = new StreamWriter(@"C:\beispieltextdatei.txt");
```

Anschließend können Sie die `Write` oder `WriteLine` Methode verwenden, um Daten in die Textdatei zu schreiben. Die `WriteLine` Methode fügt automatisch einen Zeilenumbruch hinzu, während die `Write` Methode dies nicht tut.

```C#
writer.WriteLine("Dies ist ein Beispieltext.");
writer.Write("Dies ist ein weiterer Text ohne Zeilenumbruch.");
```

Schließlich müssen Sie den `StreamWriter` schließen, um sicherzustellen, dass alle Daten erfolgreich in die Textdatei geschrieben wurden.

```C#
writer.Close();
```

Das obige Beispiel erstellt eine neue Textdatei und überschreibt alle vorhandenen Daten. Wenn Sie Daten an eine vorhandene Textdatei anhängen möchten, können Sie die `Append` Methode verwenden.

```C#
StreamWriter writer = new StreamWriter(@"C:\beispieltextdatei.txt", append: true);
```

Eine vollständige Anleitung zum Lesen und Schreiben von Textdateien in C# finden Sie in der offiziellen Microsoft-Dokumentation: [Lesen und Schreiben von Text in einer Datei](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file).

## Tiefere Einblicke

Um eine bessere Kontrolle über das Schreiben von Textdateien zu haben, können Sie auch die `File` Klasse verwenden, um direkt auf die Datei zuzugreifen, anstatt eine Instanz der `StreamWriter` Klasse zu erstellen. Dies gibt Ihnen mehr Flexibilität bei der Verwendung von Dateipfaden und beim Lesen und Schreiben von Dateien.

```C#
string path = @"C:\beispieltextdatei.txt";
File.WriteAllText(path, "Dieser Text wird in die Datei geschrieben.");
```

Wenn Sie eine vorhandene Datei anpassen und bestimmte Daten ändern möchten, bietet die `File` Klasse auch Methoden zum Ersetzen von Text in einer Datei.

Eine vollständige Dokumentation mit weiteren Beispielen finden Sie hier: [System.IO.File Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.io.file).

## Siehe auch

- [Lesen und Schreiben von Text in einer Datei](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [System.IO.File Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.io.file)