---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:23.333156-07:00
description: "Een tekstbestand schrijven betekent gegevens zoals strings opslaan in\
  \ een bestand op je schijf. Programmeurs doen dit voor logging, het opslaan van\u2026"
lastmod: '2024-03-11T00:14:24.656145-06:00'
model: gpt-4-0125-preview
summary: "Een tekstbestand schrijven betekent gegevens zoals strings opslaan in een\
  \ bestand op je schijf. Programmeurs doen dit voor logging, het opslaan van\u2026"
title: Een tekstbestand schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand schrijven betekent gegevens zoals strings opslaan in een bestand op je schijf. Programmeurs doen dit voor logging, het opslaan van configuraties, of gewoon datapersistentie.

## Hoe:
Je kunt een tekstbestand in C# schrijven met `File.WriteAllText`, `File.AppendAllText` of een `StreamWriter`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Schrijf tekst naar een nieuw bestand
        File.WriteAllText("log.txt", "Hallo, bestand!");

        // Voeg tekst toe aan het bestaande bestand
        File.AppendAllText("log.txt", "\nLaten we nog een regel toevoegen.");

        // Gebruik StreamWriter om naar een bestand te schrijven
        using (StreamWriter writer = new StreamWriter("log.txt", true))
        {
            writer.WriteLine("Nog een regel met StreamWriter.");
        }
    }
}
```

Voorbeelduitvoer in `log.txt`:
```
Hallo, bestand!
Laten we nog een regel toevoegen.
Nog een regel met StreamWriter.
```

## Diepgaande verkenning
Historisch gezien is file I/O in C# geëvolueerd van basis `FileStream` operaties tot abstracties zoals `StreamWriter`. Alternatieven omvatten het gebruik van `System.IO.FileStream` voor meer controle of asynchrone methoden zoals `WriteAllTextAsync` voor efficiëntie. `StreamWriter` gebruikt onder de motorkap een buffer om schrijfoperaties te optimaliseren.

## Zie ook
Voor gerelateerde lectuur en diepgaande tutorials:
- [MSDN Documentatie over File I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [MSDN StreamWriter-klasse](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [Tutorial over Asynchrone File I/O in C#](https://docs.microsoft.com/en-us/dotnet/standard/io/asynchronous-file-i-o)
