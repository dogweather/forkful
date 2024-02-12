---
title:                "Lavorare con i CSV"
aliases:
- it/c-sharp/working-with-csv.md
date:                  2024-02-03T19:19:32.185232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
I file CSV (Comma-Separated Values, Valori Separati da Virgola) sono un formato comune di scambio dati che rappresenta dati tabellari in testo semplice, utilizzando virgole per separare i valori individuali. I programmatori lavorano con i file CSV per importare, esportare e manipolare dati con facilità attraverso varie applicazioni e servizi, dato che è un formato semplice, ampiamente supportato e compatibile con applicazioni di fogli di calcolo, database e linguaggi di programmazione.

## Come fare:
Lavorare con i file CSV in C# può essere realizzato attraverso lo spazio dei nomi `System.IO` per operazioni di base, e per manipolazioni più complesse o per gestire file di grandi dimensioni senza interruzioni, si potrebbe considerare l'utilizzo di librerie di terze parti come `CsvHelper`. Di seguito sono riportati esempi di come leggere e scrivere file CSV utilizzando entrambi gli approcci.

### Leggere un file CSV usando System.IO
```csharp
using System;
using System.IO;

class LeggiCSV
{
    static void Main()
    {
        string percorsoFile = @"percorso\al\tuo\file.csv";
        // Lettura di tutte le righe del file CSV
        string[] righeCsv = File.ReadAllLines(percorsoFile);
        
        foreach (string riga in righeCsv)
        {
            string[] datiRiga = riga.Split(',');
            Console.WriteLine($"Prima Colonna: {datiRiga[0]}, Seconda Colonna: {datiRiga[1]}");
        }
    }
}
```

**Output dell'esempio:**
```
Prima Colonna: Nome, Seconda Colonna: Età
Prima Colonna: John Doe, Seconda Colonna: 30
```

### Scrivere un file CSV usando System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class ScriviCSV
{
    static void Main()
    {
        string percorsoFile = @"percorso\al\tuo\output.csv";
        var righe = new List<string>
        {
            "Nome,Età",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(percorsoFile, righe);
        Console.WriteLine("File CSV scritto.");
    }
}
```

**Output dell'esempio:**
```
File CSV scritto.
```

### Utilizzare CsvHelper per Leggere CSV
Per utilizzare CsvHelper, prima, aggiungi il pacchetto `CsvHelper` al tuo progetto usando NuGet Package Manager.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class LeggiCSVConCsvHelper
{
    static void Main()
    {
        string percorsoFile = @"percorso\al\tuo\file.csv";

        using (var lettore = new StreamReader(percorsoFile))
        using (var csv = new CsvReader(lettore, CultureInfo.InvariantCulture))
        {
            var record = csv.GetRecords<dynamic>().ToList();
            foreach (var record in record)
            {
                Console.WriteLine($"Prima Colonna: {record.Name}, Seconda Colonna: {record.Age}");
            }
        }
    }
}
```

**Output dell'esempio:**
```
Prima Colonna: John Doe, Seconda Colonna: 30
Prima Colonna: Jane Smith, Seconda Colonna: 25
```

### Utilizzare CsvHelper per Scrivere CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class ScriviCSVConCsvHelper
{
    public class Persona
    {
        public string Nome { get; set; }
        public int Età { get; set; }
    }

    static void Main()
    {
        string percorsoFile = @"percorso\al\tuo\output.csv";
        var record = new List<Persona>
        {
            new Persona { Nome = "John Doe", Età = 30 },
            new Persona { Nome = "Jane Smith", Età = 25 }
        };

        using (var scrittore = new StreamWriter(percorsoFile))
        using (var csv = new CsvWriter(scrittore, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(record);
        }
        
        Console.WriteLine("File CSV scritto con CsvHelper.");
    }
}
```

**Output dell'esempio:**
```
File CSV scritto con CsvHelper.
```
