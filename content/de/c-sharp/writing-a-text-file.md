---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben in eine Textdatei ermöglicht die dauerhafte Datenspeicherung. Programmierer nutzen dies, um Daten zu persistieren, Einstellungen zu speichern oder Log-Informationen auszugeben.

## How to:

Datei schreiben:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = "Beispiel.txt";
        string content = "Hallo, das ist ein Textfile!";

        File.WriteAllText(path, content);
        Console.WriteLine("Datei geschrieben!");
    }
}
```

Ausgabe:
```
Datei geschrieben!
```

Datei anhängen:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = "Beispiel.txt";
        string appendContent = "\nDas ist ein angehängter Text.";

        File.AppendAllText(path, appendContent);
        Console.WriteLine("Text angehängt.");
    }
}
```

Ausgabe:
```
Text angehängt.
```

## Deep Dive

Früher wurde `StreamWriter` oder `FileStream` für das Schreiben in Dateien genutzt. Heute verwendet man oft `File` oder `FileInfo` für einfachere Aufgaben und `StreamWriter` für komplexere Anforderungen, wie z.B. wenn ein Text schrittweise in eine Datei geschrieben werden soll.

Alternativen zu `File.WriteAllText` und `File.AppendAllText` sind z.B. `File.WriteAllLines` oder `File.WriteAllBytes` - abhängig vom gewünschten Dateiinhalt.

Beim Schreiben von Daten auf die Festplatte sollte Exception Handling (z.B. `try-catch`-Blöcke) verwendet werden, um auf Fehler wie `IOException` reagieren zu können.

## See Also

- Microsoft Docs zu `System.IO.File`: https://docs.microsoft.com/de-de/dotnet/api/system.io.file?view=net-6.0
- Microsoft Docs zu `StreamWriter`: https://docs.microsoft.com/de-de/dotnet/api/system.io.streamwriter?view=net-6.0
- C# Einführung in Exception Handling: https://docs.microsoft.com/de-de/dotnet/csharp/fundamentals/exceptions/exception-handling
