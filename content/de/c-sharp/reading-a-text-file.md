---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien zu lesen bedeutet, den Inhalt einer Textdatei in das Speichermodell deines Programms zu laden. Programmierer tun dies häufig, um Daten für ihre Applikationen zu laden oder um mit externen Dateien zu interagieren.

## So geht's:
Hier ist ein einfacher Code, um eine Textdatei in C# zu lesen. Der Folgende Code liest eine vorhandene Datei namens `Datei.txt` und gibt jeden Zeile auf der Konsole aus.

```C#
using System.IO;

string[] lines = File.ReadAllLines(@"C:\PfadZurDatei\Datei.txt");

foreach (string line in lines)
{
    Console.WriteLine(line);
}
```
Wenn die `Datei.txt` zum Beispiel folgenden Inhalt hat: 

```
Hallo Welt!
Das ist eine Textdatei.
```

Der Ausgang wäre dann:

```
Hallo Welt!
Das ist eine Textdatei.
```
## Tiefer eintauchen:
Historisch gesehen, waren Textdateien eine der ersten Methoden zur Speicherung von digitalen Informationen und sind immer noch beliebt aufgrund ihrer Einfachheit und Flexibilität. Alternative Methoden zum Lesen von Textdateien in C# beinhalten den `StreamReader` oder das asynchrone Lesen mit `ReadAllLinesAsync`.

Das `File.ReadAllLines` ist eine High-Level-Methode, die die Datei öffnet, jede Zeile liest und dann die Datei schließt. Es ist einfach zu benutzen, aber nicht so flexibel wie ein `StreamReader`, der mehr Kontrolle ermöglicht, aber ebenso komplizierter ist.

## Siehe Auch:
- [Microsoft Dokumentation: Textdateien lesen](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [DotNetPerls: Textdateien lesen](https://www.dotnetperls.com/readfile)
- [Stackoverflow: Alternativen zum Lesen von Textdateien](https://stackoverflow.com/questions/507863/when-to-use-file-readalllines-over-streamreader-in-c)