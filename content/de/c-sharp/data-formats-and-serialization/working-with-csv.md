---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:14.618285-07:00
description: "CSV-Dateien (Comma-Separated Values), also durch Kommata getrennte Werte,\
  \ sind ein \xFCbliches Datenformat f\xFCr den Austausch, das tabellarische Daten\
  \ in\u2026"
lastmod: '2024-03-13T22:44:53.911088-06:00'
model: gpt-4-0125-preview
summary: "CSV-Dateien (Comma-Separated Values), also durch Kommata getrennte Werte,\
  \ sind ein \xFCbliches Datenformat f\xFCr den Austausch, das tabellarische Daten\
  \ in\u2026"
title: Arbeiten mit CSV
weight: 37
---

## Was & Warum?
CSV-Dateien (Comma-Separated Values), also durch Kommata getrennte Werte, sind ein übliches Datenformat für den Austausch, das tabellarische Daten in reinem Text darstellt, wobei Kommata verwendet werden, um einzelne Werte voneinander zu trennen. Programmierer arbeiten mit CSV-Dateien, um Daten einfach zu importieren, zu exportieren und zu manipulieren, und dies über verschiedene Anwendungen und Dienste hinweg, da es ein einfaches, weit verbreitetes Format ist, das mit Tabellenkalkulationsanwendungen, Datenbanken und Programmiersprachen kompatibel ist.

## Wie geht das:
Die Arbeit mit CSV-Dateien in C# kann durch den `System.IO`-Namespace für grundlegende Operationen erreicht werden, und für komplexere Manipulationen oder um größere Dateien nahtlos zu handhaben, könnte man Drittanbieter-Bibliotheken wie `CsvHelper` in Betracht ziehen. Unten sind Beispiele, wie man CSV-Dateien liest und in sie schreibt, unter Verwendung beider Ansätze.

### Eine CSV-Datei lesen mit System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"Pfad\zu\deiner\Datei.csv";
        // Alle Zeilen der CSV-Datei lesen
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Erste Spalte: {rowData[0]}, Zweite Spalte: {rowData[1]}");
        }
    }
}
```

**Beispielausgabe:**
```
Erste Spalte: Name, Zweite Spalte: Alter
Erste Spalte: John Doe, Zweite Spalte: 30
```

### In eine CSV-Datei schreiben mit System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"Pfad\zu\deiner\Ausgabe.csv";
        var lines = new List<string>
        {
            "Name,Alter",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV-Datei geschrieben.");
    }
}
```

**Beispielausgabe:**
```
CSV-Datei geschrieben.
```

### CSV lesen mit CsvHelper
Um CsvHelper zu verwenden, füge zunächst das `CsvHelper` Paket zu deinem Projekt hinzu, indem du den NuGet-Paket-Manager verwendest.

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
        string filePath = @"Pfad\zu\deiner\Datei.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Erste Spalte: {record.Name}, Zweite Spalte: {record.Age}");
            }
        }
    }
}
```

**Beispielausgabe:**
```
Erste Spalte: John Doe, Zweite Spalte: 30
Erste Spalte: Jane Smith, Zweite Spalte: 25
```

### CSV schreiben mit CsvHelper
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
        string filePath = @"Pfad\zu\deiner\Ausgabe.csv";
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
        
        Console.WriteLine("CSV-Datei geschrieben mit CsvHelper.");
    }
}
```

**Beispielausgabe:**
```
CSV-Datei geschrieben mit CsvHelper.
```
