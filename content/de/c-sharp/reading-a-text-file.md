---
title:                "Das Lesen einer Textdatei"
html_title:           "C#: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal eine Textdatei gelesen hast, weißt du, wie nützlich es ist, Informationen aus einer Datei auszulesen. In diesem Artikel werden wir besprechen, wie du in C# eine Textdatei lesen kannst.

## Wie geht das?

Um eine Textdatei in C# zu lesen, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der `StreamReader`-Klasse. In diesem Beispiel zeigen wir, wie du mithilfe der `StreamReader`-Klasse eine Textdatei zeilenweise auslesen kannst:

```C#
using System;
using System.IO;

StreamReader reader = new StreamReader("beispiel.txt");

while (!reader.EndOfStream)
{
    Console.WriteLine(reader.ReadLine());
}

reader.Close();
```

Nun nehmen wir an, unsere Textdatei `beispiel.txt` enthält die folgenden Zeilen:

```
Dies ist eine Beispieltextdatei.
Hier sind ein paar Zeilen.
Und hier noch eine weitere Zeile.
```

Wenn wir unser oben genanntes Beispielprogramm ausführen, wird die folgende Ausgabe erzeugt:

```
Dies ist eine Beispieltextdatei.
Hier sind ein paar Zeilen.
Und hier noch eine weitere Zeile.
```

Die `StreamReader`-Klasse ermöglicht es uns, eine Textdatei zu öffnen, zu lesen und am Ende wieder zu schließen.

## Tiefentauchen

Es gibt noch viele weitere Methoden und Möglichkeiten, um Textdateien in C# zu lesen. Wir können auch die `File`-Klasse verwenden, um beispielsweise den gesamten Inhalt einer Datei auf einmal auszulesen und in eine Liste zu speichern.

```C#
using System;
using System.IO;

List<string> lines = new List<string>(File.ReadAllLines("beispiel.txt"));

foreach (var line in lines)
{
    Console.WriteLine(line);
}
```

Dies würde die gleiche Ausgabe erzeugen wie das vorherige Beispiel. Beachte jedoch, dass wir hier nicht die `EndOfStream`-Methode verwenden müssen, da die `File`-Klasse den gesamten Inhalt der Datei auf einmal liest.

Es gibt noch viele weitere Details über das Lesen von Textdateien in C#, die wir hier nicht abdecken konnten. Wenn du mehr darüber erfahren möchtest, empfehlen wir dir, die offizielle Dokumentation durchzulesen und mit eigenen Experimenten zu lernen.

## Siehe auch

- [C# Dokumentation - Lesen von Textdateien](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file)
- [C# Dokumentation - StreamReader-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# Dokumentation - File-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.io.file?view=net-5.0)