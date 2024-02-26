---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:43.162345-07:00
description: "CSV-filer (Comma-Separated Values, verdier skilt med komma) er et vanlig\
  \ datautvekslingsformat som representerer tabelldata i ren tekst ved \xE5 bruke\u2026"
lastmod: '2024-02-25T18:49:38.993670-07:00'
model: gpt-4-0125-preview
summary: "CSV-filer (Comma-Separated Values, verdier skilt med komma) er et vanlig\
  \ datautvekslingsformat som representerer tabelldata i ren tekst ved \xE5 bruke\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV-filer (Comma-Separated Values, verdier skilt med komma) er et vanlig datautvekslingsformat som representerer tabelldata i ren tekst ved å bruke kommaer for å skille individuelle verdier. Programmerere arbeider med CSV-filer for å importere, eksportere, og manipulere data enkelt på tvers av ulike applikasjoner og tjenester, siden det er et enkelt, bredt støttet format som er kompatibelt med regnearkprogrammer, databaser og programmeringsspråk.

## Hvordan:
Arbeid med CSV-filer i C# kan gjøres gjennom `System.IO` navneområdet for grunnleggende operasjoner, og for mer komplekse manipulasjoner eller for å håndtere større filer sømløst, kan man vurdere tredjepartsbiblioteker som `CsvHelper`. Nedenfor er eksempler på hvordan man leser fra og skriver til CSV-filer ved hjelp av begge tilnærminger.

### Lese en CSV-fil ved hjelp av System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"sti\til\din\fil.csv";
        // Lese alle linjene i CSV-filen
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Første kolonne: {rowData[0]}, Andre kolonne: {rowData[1]}");
        }
    }
}
```

**Eksempel på utdata:**
```
Første kolonne: Navn, Andre kolonne: Alder
Første kolonne: John Doe, Andre kolonne: 30
```

### Skrive til en CSV-fil ved hjelp av System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"sti\til\din\output.csv";
        var lines = new List<string>
        {
            "Navn,Alder",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV-filen er skrevet.");
    }
}
```

**Eksempel på utdata:**
```
CSV-filen er skrevet.
```

### Bruke CsvHelper til å lese CSV
For å bruke CsvHelper, legg først til `CsvHelper`-pakken til prosjektet ditt ved bruk av NuGet Package Manager.

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
        string filePath = @"sti\til\din\fil.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Første kolonne: {record.Name}, Andre kolonne: {record.Age}");
            }
        }
    }
}
```

**Eksempel på utdata:**
```
Første kolonne: John Doe, Andre kolonne: 30
Første kolonne: Jane Smith, Andre kolonne: 25
```

### Bruke CsvHelper til å skrive CSV
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
        public string Navn { get; set; }
        public int Alder { get; set; }
    }

    static void Main()
    {
        string filePath = @"sti\til\din\output.csv";
        var records = new List<Person>
        {
            new Person { Navn = "John Doe", Alder = 30 },
            new Person { Navn = "Jane Smith", Alder = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CSV-filen er skrevet med CsvHelper.");
    }
}
```

**Eksempel på utdata:**
```
CSV-filen er skrevet med CsvHelper.
```
