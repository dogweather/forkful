---
date: 2024-01-20 17:40:18.869683-07:00
description: "How To: Hier ist ein einfaches Beispiel in C#, wie man eine tempor\xE4\
  re Datei erstellt."
lastmod: '2024-03-13T22:44:53.907961-06:00'
model: gpt-4-1106-preview
summary: "Hier ist ein einfaches Beispiel in C#, wie man eine tempor\xE4re Datei erstellt."
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

## How To:
Hier ist ein einfaches Beispiel in C#, wie man eine temporäre Datei erstellt.

```csharp
using System;
using System.IO;

class TemporaryFilesExample
{
    static void Main()
    {
        string tempFilePath = Path.GetTempFileName();
        Console.WriteLine("Temporäre Datei erstellt bei: " + tempFilePath);

        // Zum Demonstrieren, dass die Datei existiert:
        if (File.Exists(tempFilePath))
        {
            Console.WriteLine("Datei existiert. Schreibe etwas hinein...");
            File.WriteAllText(tempFilePath, "Hello, temporary world!");
        }

        // Temporäre Datei verwenden...
        
        // Temporäre Datei löschen
        File.Delete(tempFilePath);
        Console.WriteLine("Temporäre Datei gelöscht.");

        // Warten, damit Output im Konsolenfenster sichtbar bleibt.
        Console.ReadKey();
    }
}
```

Das wäre die Konsolenausgabe:
```
Temporäre Datei erstellt bei: C:\Users\[Benutzername]\AppData\Local\Temp\tmpA4B2.tmp
Datei existiert. Schreibe etwas hinein...
Temporäre Datei gelöscht.
```

## Deep Dive:
Bevor Dateisysteme ausgefeilt waren, hatten temporäre Dateien eine essenzielle Rolle bei der Verwaltung des begrenzten Speicherplatzes. Heute helfen sie, die Integrität und Leistung durch den Umgang mit vorübergehenden Daten bei Bedarf zu sichern.

Alternativen zur `Path.GetTempFileName()` könnten das manuelle Erstellen von Dateien in einem selbst definierten temporären Verzeichnis sein oder das Verwenden von Streams, die nie auf die Festplatte schreiben (`MemoryStream`).

Die Methode `Path.GetTempFileName()` erstellt eine einzigartige Datei im temporären Verzeichnis des Systems und gibt den Pfad zurück. Die Datei wird sofort erzeugt, damit der Name sicher reserviert ist. Nach Gebrauch sollte die Datei gelöscht werden, um das Dateisystem sauber zu halten.

## See Also:
- [Path.GetTempFileName Method in der .NET-Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.io.path.gettempfilename)
- [File.Delete Method in der .NET-Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.io.file.delete)
