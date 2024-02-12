---
title:                "Textdatei einlesen"
aliases:
- /de/c-sharp/reading-a-text-file/
date:                  2024-01-20T17:54:00.454383-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Dateien lesen bedeutet, Daten aus einer Textdatei in den Speicher zu laden, damit Code sie verarbeiten kann. Programmierer machen das, um Inhalte zu verarbeiten, Konfigurationen zu laden oder Daten zu speichern.

## How to:
Bist du bereit, eine Textdatei in C# zu lesen? Hier sind ein paar schlanke Beispiele:

### Datei mit `File.ReadAllLines()` lesen:
```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string path = "Beispiel.txt";
        string[] lines = File.ReadAllLines(path);
        
        foreach (string line in lines) {
            Console.WriteLine(line);
        }
    }
}
```
**Ausgabe:**
```
Zeile 1
Zeile 2
Zeile 3
```

### Datei mit `StreamReader` lesen:
```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string path = "Beispiel.txt";
        
        using (StreamReader reader = new StreamReader(path)) {
            string line;
            while ((line = reader.ReadLine()) != null) {
                Console.WriteLine(line);
            }
        }
    }
}
```
**Ausgabe:**
```
Zeile 1
Zeile 2
Zeile 3
```

## Deep Dive
Früher, vor .NET, war das Lesen von Dateien umständlicher. Mit .NET kam `System.IO`, das die Arbeit stark vereinfacht hat. Alternativen zu den obigen Methoden sind `File.ReadAllText()` und asynchrone Ansätze wie `File.ReadAllLinesAsync()` oder das asynchrone Arbeiten mit `StreamReader`. Beim Lesen großer Dateien ist `StreamReader` vorzuziehen, da es nicht den ganzen Inhalt auf einmal in den Speicher lädt. Async-Methoden helfen dabei, die Anwendung reaktionsfähig zu halten, besonders in UI-Anwendungen oder bei Netzwerk-IO.

## See Also
- Microsoft Docs zu `File`: https://docs.microsoft.com/en-us/dotnet/api/system.io.file
- Microsoft Docs zu `StreamReader`: https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader
- Ein Tutorial zum asynchronen Lesen von Dateien: https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file
