---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavoriamo con i file CSV per gestire dati in formato testuale semplificato, ottimo per esportazione e importazione. I programmatori li usano per la loro flessibilità e compatibilità universale con sistemi e applicazioni.

## How to:
Per leggere un file CSV in C#, utilizziamo `StreamReader`. Per scriverne uno, `StreamWriter`. Vediamo come.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Leggiamo un CSV
        var fileDiLettura = "dati.csv";
        using (var reader = new StreamReader(fileDiLettura))
        {
            while (!reader.EndOfStream)
            {
                var line = reader.ReadLine();
                var values = line.Split(',');
                Console.WriteLine($"Nome: {values[0]}, Età: {values[1]}");
            }
        }

        // Scriviamo in un CSV
        var fileDiScrittura = "export.csv";
        using (var writer = new StreamWriter(fileDiScrittura))
        {
            writer.WriteLine("Nome,Età");
            writer.WriteLine("Mario,30");
            writer.WriteLine("Luigi,28");
        }
    }
}
```

Output di lettura:
```
Nome: Mario, Età: 30
Nome: Luigi, Età: 28
```

## Deep Dive
Nato negli anni '70, il formato CSV diventò popolare con l'introduzione di fogli di calcolo come Excel. Alternativamente, possiamo usare formati come JSON o XML, che si prestano meglio ai dati più complessi. Implementare la gestione dei CSV in C# è semplice, ma prestare attenzione alle virgole nei dati e ai dati multilinea è cruciale per evitare errori di parsing.

## See Also
- Documentazione Microsoft su `StreamReader` e `StreamWriter`: [StreamReader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1), [StreamWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=netcore-3.1)
- [RFC 4180](https://tools.ietf.org/html/rfc4180): Standard del formato CSV.
- Libreria per la gestione avanzata dei CSV in C#: [CsvHelper](https://joshclose.github.io/CsvHelper/)
