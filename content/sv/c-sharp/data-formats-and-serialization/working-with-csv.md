---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:19.790459-07:00
description: "CSV-filer (Comma-Separated Values) \xE4r ett vanligt datautbytesformat\
  \ som representerar tabul\xE4r data i ren text, genom att anv\xE4nda kommatecken\
  \ f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.935853-06:00'
model: gpt-4-0125-preview
summary: "CSV-filer (Comma-Separated Values) \xE4r ett vanligt datautbytesformat som\
  \ representerar tabul\xE4r data i ren text, genom att anv\xE4nda kommatecken f\xF6\
  r att\u2026"
title: Arbeta med CSV
---

{{< edit_this_page >}}

## Vad och varför?
CSV-filer (Comma-Separated Values) är ett vanligt datautbytesformat som representerar tabulär data i ren text, genom att använda kommatecken för att separera enskilda värden. Programmerare arbetar med CSV-filer för att importera, exportera och manipulera data enkelt över olika applikationer och tjänster, eftersom det är ett enkelt, brett stöttat format som är kompatibelt med kalkylapplikationer, databaser och programmeringsspråk.

## Hur man gör:
Att arbeta med CSV-filer i C# kan åstadkommas genom namnrymden `System.IO` för grundläggande operationer, och för mer komplexa manipulationer eller för att hantera större filer sömlöst, kan man överväga tredjepartsbibliotek som `CsvHelper`. Nedan finns exempel på hur man läser från och skriver till CSV-filer med båda metoderna.

### Läsa en CSV-fil med System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"sökväg\till\din\fil.csv";
        // Läser alla rader i CSV-filen
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Första kolumnen: {rowData[0]}, Andra kolumnen: {rowData[1]}");
        }
    }
}
```

**Exempelutskrift:**
```
Första kolumnen: Namn, Andra kolumnen: Ålder
Första kolumnen: John Doe, Andra kolumnen: 30
```

### Skriva till en CSV-fil med System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"sökväg\till\din\utdata.csv";
        var lines = new List<string>
        {
            "Namn,Ålder",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV-fil skriven.");
    }
}
```

**Exempelutskrift:**
```
CSV-fil skriven.
```

### Använda CsvHelper för att läsa CSV
För att använda CsvHelper, lägg först till `CsvHelper`-paketet till ditt projekt med NuGet Package Manager.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"sökväg\till\din\fil.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Första kolumnen: {record.Name}, Andra kolumnen: {record.Age}");
            }
        }
    }
}
```

**Exempelutskrift:**
```
Första kolumnen: John Doe, Andra kolumnen: 30
Första kolumnen: Jane Smith, Andra kolumnen: 25
```

### Använda CsvHelper för att skriva CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Namn { get; set; }
        public int Ålder { get; set; }
    }

    static void Main()
    {
        string filePath = @"sökväg\till\din\utdata.csv";
        var records = new List<Person>
        {
            new Person { Namn = "John Doe", Ålder = 30 },
            new Person { Namn = "Jane Smith", Ålder = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CSV-fil skriven med CsvHelper.");
    }
}
```

**Exempelutskrift:**
```
CSV-fil skriven med CsvHelper.
```
