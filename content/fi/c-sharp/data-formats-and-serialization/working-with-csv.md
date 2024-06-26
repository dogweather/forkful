---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:48.960918-07:00
description: "Kuinka: CSV-tiedostojen k\xE4sittely C#:ssa voidaan suorittaa `System.IO`\
  \ nimiavaruuden avulla perustoimintoja varten, ja monimutkaisempien manipulointien\u2026"
lastmod: '2024-03-13T22:44:56.596510-06:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen k\xE4sittely C#:ssa voidaan suorittaa `System.IO` nimiavaruuden\
  \ avulla perustoimintoja varten, ja monimutkaisempien manipulointien tai suurempien\
  \ tiedostojen k\xE4sittelyyn sujuvasti voi harkita kolmannen osapuolen kirjastoja,\
  \ kuten `CsvHelper`."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:
CSV-tiedostojen käsittely C#:ssa voidaan suorittaa `System.IO` nimiavaruuden avulla perustoimintoja varten, ja monimutkaisempien manipulointien tai suurempien tiedostojen käsittelyyn sujuvasti voi harkita kolmannen osapuolen kirjastoja, kuten `CsvHelper`. Alla on esimerkkejä, kuinka lukea ja kirjoittaa CSV-tiedostoja käyttäen molempia lähestymistapoja.

### CSV-tiedoston lukeminen käyttäen System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"polku\tiedostoonne.csv";
        // Lukee kaikki CSV-tiedoston rivit
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Ensimmäinen sarake: {rowData[0]}, Toinen sarake: {rowData[1]}");
        }
    }
}
```

**Esimerkkitulostus:**
```
Ensimmäinen sarake: Nimi, Toinen sarake: Ikä
Ensimmäinen sarake: John Doe, Toinen sarake: 30
```

### Kirjoittaminen CSV-tiedostoon käyttäen System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"polku\tulostiedostoonne.csv";
        var lines = new List<string>
        {
            "Nimi,Ikä",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV-tiedosto kirjoitettu.");
    }
}
```

**Esimerkkitulostus:**
```
CSV-tiedosto kirjoitettu.
```

### CsvHelper:n käyttö CSV:n lukemiseen
CsvHelper:n käyttämiseksi, lisää ensin `CsvHelper` paketti projektiisi käyttäen NuGet Package Manageria.

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
        string filePath = @"polku\tiedostoonne.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Ensimmäinen sarake: {record.Name}, Toinen sarake: {record.Age}");
            }
        }
    }
}
```

**Esimerkkitulostus:**
```
Ensimmäinen sarake: John Doe, Toinen sarake: 30
Ensimmäinen sarake: Jane Smith, Toinen sarake: 25
```

### CsvHelper:n käyttö CSV:n kirjoittamiseen
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Henkilö
    {
        public string Nimi { get; set; }
        public int Ikä { get; set; }
    }

    static void Main()
    {
        string filePath = @"polku\tulostiedostoonne.csv";
        var records = new List<Henkilö>
        {
            new Henkilö { Nimi = "John Doe", Ikä = 30 },
            new Henkilö { Nimi = "Jane Smith", Ikä = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CSV-tiedosto kirjoitettu CsvHelperin avulla.");
    }
}
```

**Esimerkkitulostus:**
```
CSV-tiedosto kirjoitettu CsvHelperin avulla.
```
