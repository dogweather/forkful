---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:16.462698-07:00
description: "Jak to zrobi\u0107: Praca z plikami CSV w C# mo\u017Ce by\u0107 realizowana\
  \ za pomoc\u0105 przestrzeni nazw `System.IO` dla podstawowych operacji, a w przypadku\
  \ bardziej\u2026"
lastmod: '2024-03-13T22:44:35.431420-06:00'
model: gpt-4-0125-preview
summary: "Praca z plikami CSV w C# mo\u017Ce by\u0107 realizowana za pomoc\u0105 przestrzeni\
  \ nazw `System.IO` dla podstawowych operacji, a w przypadku bardziej skomplikowanych\
  \ manipulacji lub obs\u0142ugi wi\u0119kszych plik\xF3w \u0142adnie, mo\u017Cna\
  \ rozwa\u017Cy\u0107 u\u017Cycie bibliotek firm trzecich, takich jak `CsvHelper`."
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
Praca z plikami CSV w C# może być realizowana za pomocą przestrzeni nazw `System.IO` dla podstawowych operacji, a w przypadku bardziej skomplikowanych manipulacji lub obsługi większych plików ładnie, można rozważyć użycie bibliotek firm trzecich, takich jak `CsvHelper`. Poniżej znajdują się przykłady, jak czytać z plików CSV i jak do nich zapisywać, korzystając z obu podejść.

### Czytanie pliku CSV za pomocą System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // Odczytywanie wszystkich linii pliku CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Pierwsza kolumna: {rowData[0]}, Druga kolumna: {rowData[1]}");
        }
    }
}
```

**Przykładowe wyjście:**
```
Pierwsza kolumna: Name, Druga kolumna: Age
Pierwsza kolumna: John Doe, Druga kolumna: 30
```

### Zapisywanie do pliku CSV za pomocą System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var lines = new List<string>
        {
            "Name,Age",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("Plik CSV został zapisany.");
    }
}
```

**Przykładowe wyjście:**
```
Plik CSV został zapisany.
```

### Użycie CsvHelper do czytania CSV
Aby użyć CsvHelper, najpierw dodaj pakiet `CsvHelper` do swojego projektu przy użyciu Menedżera Pakietów NuGet.

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
        string filePath = @"path\to\your\file.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Pierwsza kolumna: {record.Name}, Druga kolumna: {record.Age}");
            }
        }
    }
}
```

**Przykładowe wyjście:**
```
Pierwsza kolumna: John Doe, Druga kolumna: 30
Pierwsza kolumna: Jane Smith, Druga kolumna: 25
```

### Użycie CsvHelper do zapisywania CSV
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
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { Name = "John Doe", Age = 30 },
            new Person { Name = "Jane Smith", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("Plik CSV został zapisany przy użyciu CsvHelper.");
    }
}
```

**Przykładowe wyjście:**
```
Plik CSV został zapisany przy użyciu CsvHelper.
```
