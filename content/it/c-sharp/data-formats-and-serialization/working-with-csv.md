---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:32.185232-07:00
description: "Come fare: Lavorare con i file CSV in C# pu\xF2 essere realizzato attraverso\
  \ lo spazio dei nomi `System.IO` per operazioni di base, e per manipolazioni pi\xF9\
  \u2026"
lastmod: '2024-03-13T22:44:43.455715-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con i file CSV in C# pu\xF2 essere realizzato attraverso lo spazio\
  \ dei nomi `System.IO` per operazioni di base, e per manipolazioni pi\xF9 complesse\
  \ o per gestire file di grandi dimensioni senza interruzioni, si potrebbe considerare\
  \ l'utilizzo di librerie di terze parti come `CsvHelper`."
title: Lavorare con i CSV
weight: 37
---

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
