---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.585437-07:00
description: Hoe te **Voorbeeld van uitvoer:**.
lastmod: '2024-04-05T21:53:50.855897-06:00'
model: gpt-4-0125-preview
summary: ''
title: Werken met CSV
weight: 37
---

## Hoe te


### CSV-bestanden lezen
```C#
using System;
using System.IO;

class ReadCSVExample
{
    static void Main()
    {
        string path = "data.csv";
        using (var reader = new StreamReader(path))
        {
            while (!reader.EndOfStream)
            {
                var line = reader.ReadLine();
                var values = line.Split(',');
                // Doe nu iets met de waarden, bijv. ze afdrukken
                Console.WriteLine(String.Join(" | ", values));
            }
        }
    }
}
```
**Voorbeeld van uitvoer:**
```
John | Doe | johndoe@example.com
Jane | Smith | janesmith@example.com
```

### CSV-bestanden schrijven
```C#
using System;
using System.IO;

class WriteCSVExample
{
    static void Main()
    {
        string path = "output.csv";
        var records = new[]
        {
            new[] {"Naam", "Leeftijd", "E-mail"},
            new[] {"Alice", "23", "alice@example.com"},
            new[] {"Bob", "30", "bob@example.com"}
        };

        using (var writer = new StreamWriter(path))
        {
            foreach (var record in records)
            {
                var line = String.Join(",", record);
                writer.WriteLine(line);
            }
        }
        Console.WriteLine($"Gegevens geschreven naar {path}");
    }
}
```
**Voorbeeld van uitvoer:**
```
Gegevens geschreven naar output.csv
```

## Diepere Duik
CSV bestaat al sinds de vroege dagen van het rekenen en overbrugt de kloof tussen diverse systemen. Het is niet perfect — mist standaardcodering voor karakters en ondersteunt meerdere regelvelden niet goed zonder een robuuste parser. Dat is waar formaten zoals JSON en XML binnenkomen, die meer complexiteit bieden, maar betere structuur voor hiërarchische gegevens.

Onder de motorkap ben je meestal strings aan het manipuleren, ofwel ingebouwde `string` methodes of bibliotheken zoals `CsvHelper` kunnen extra kracht toevoegen aan je CSV-behandeling, met meer functies en het sierlijk afhandelen van randgevallen. Onthoud, er is geen native CSV-behandeling in .NET, dus je bent op jezelf aangewezen met stringmanipulatie of je kunt kiezen voor een externe bibliotheek.

## Zie Ook
Voor meer diepgaande CSV-manipulatie in C#:
- [CsvHelper Bibliotheek](https://joshclose.github.io/CsvHelper/)
- [Microsoft's documentatie over `StreamReader`](https://docs.microsoft.com/nl-nl/dotnet/api/system.io.streamreader)

Leer meer over alternatieven voor CSV:
- [JSON Begrijpen](https://www.json.org/json-nl.html)
- [XML in een Notendop](https://www.w3schools.com/xml/xml_whatis.asp)
