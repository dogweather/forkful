---
title:                "C#: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Lesen von Textdateien ist eine grundlegende und wichtige Fähigkeit in der Programmierung. Es ermöglicht uns, Informationen aus einer externen Datei in unser Programm zu laden und sie zu verarbeiten. Dies kann hilfreich sein, um große Datenmengen zu verarbeiten oder um die Benutzerfreundlichkeit unserer Anwendung zu verbessern, indem wir Konfigurationen oder Einstellungen aus einer Datei lesen.

## Wie macht man es?
Das Lesen von Textdateien in C# ist relativ einfach. Wir verwenden dazu die Klasse `StreamReader` aus dem Namespace `System.IO`. Zunächst müssen wir einen `StreamReader`-Objekt erstellen und ihm den Pfad zur Datei übergeben, die wir lesen möchten. Anschließend können wir die Methode `ReadLine()` verwenden, um jeweils eine Zeile aus der Datei zu lesen. Wir schreiben einfach eine Schleife, die solange läuft, bis das Ende der Datei erreicht ist, und jedes Mal die gelesene Zeile ausgibt. Hier ist ein Beispielcode:

```C#
using System;
using System.IO;

namespace LesenTextdatei
{
    class Program
    {
        static void Main(string[] args)
        {
            // Erstelle einen StreamReader und weise ihm den Pfad zur Datei zu
            StreamReader reader = new StreamReader(@"C:\MeineDatei.txt");
            
            // Schleife, die solange läuft bis das Ende der Datei erreicht ist
            while (!reader.EndOfStream)
            {
                // Lese eine Zeile aus der Datei und gib sie aus
                string line = reader.ReadLine();
                Console.WriteLine(line);
            }
            
            // Schließe den StreamReader
            reader.Close();
            
           Console.ReadKey();
        }
    }
}
```

Die Ausgabe wird je nach Inhalt der Textdatei unterschiedlich sein. Hier ist ein Beispiel für eine Textdatei mit den Inhalten "Hallo" und "Welt":

```shell
Hallo
Welt
```

## Tiefer in die Materie eintauchen
Um eine Textdatei in C# zu lesen, gibt es einige zusätzliche Dinge zu beachten. Zum Beispiel müssen wir darauf achten, die richtige Codierung (z.B. UTF-8 oder ANSI) zu verwenden, um die Datei richtig zu lesen. Auch die Verwendung von `using`-Blöcken ist empfehlenswert, um sicherzustellen, dass der `StreamReader` ordnungsgemäß geschlossen wird, selbst wenn während des Lesens eine Ausnahme auftritt.

Eine weitere Sache, die man wissen sollte, ist, wie man Daten aus einer Textdatei verarbeitet. Man kann beispielsweise eine Liste erstellen und jeweils die gelesenen Zeilen in die Liste hinzufügen, um später darauf zuzugreifen.

## Siehe auch
- [Dokumentation von StreamReader (Microsoft)](https://docs.microsoft.com/de-de/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# Tutorial: Lesen von Textdateien (Tutorialspoint)](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)
- [Die verschiedenen Codierungen verstehen (Microsoft)](https://docs.microsoft.com/de-de/dotnet/standard/base-types/character-encoding)