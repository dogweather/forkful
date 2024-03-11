---
date: 2024-01-20 17:40:18.869683-07:00
description: "Ein tempor\xE4res File ist eine Datei, die zur kurzzeitigen Datenspeicherung\
  \ w\xE4hrend der Laufzeit eines Programms erstellt wird. Programmierer nutzen solche\u2026"
lastmod: '2024-03-11T00:14:27.803772-06:00'
model: gpt-4-1106-preview
summary: "Ein tempor\xE4res File ist eine Datei, die zur kurzzeitigen Datenspeicherung\
  \ w\xE4hrend der Laufzeit eines Programms erstellt wird. Programmierer nutzen solche\u2026"
title: "Erstellung einer tempor\xE4ren Datei"
---

{{< edit_this_page >}}

## Was & Warum?
Ein temporäres File ist eine Datei, die zur kurzzeitigen Datenspeicherung während der Laufzeit eines Programms erstellt wird. Programmierer nutzen solche temporären Dateien für vielfältige Zwecke, beispielsweise zum sicheren Umgang mit großen Datenmengen, die nicht im Hauptspeicher gehalten werden können, oder zum Erstellen von Backup-Kopien.

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
