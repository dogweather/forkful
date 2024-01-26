---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec des CSV en C#, c’est manipuler des données structurées en texte simple. C'est pratique pour l’import/export entre différents logiciels.

## How to:
```C#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var csvContent = File.ReadAllLines("exemplo.csv");
        var data = from line in csvContent.Skip(1) // Skip CSV header
                   let columns = line.Split(',')
                   select new
                   {
                       Name = columns[0],
                       Age = int.Parse(columns[1])
                   };

        foreach (var record in data)
            Console.WriteLine($"Nom: {record.Name}, Âge: {record.Age}");
    }
}
```
Sortie:
```
Nom: Jean, Âge: 30
Nom: Marie, Âge: 22
```

## Deep Dive
CSV est l'acronyme de Comma-Separated Values, apparu dans les années 1970. Alternatives: JSON, XML. Pourtant, CSV reste apprécié pour sa simplicité. Pour une implémentation robuste et complexe, considérez `TextFieldParser` dans .NET ou la bibliothèque `CsvHelper`.

## See Also
- [CSVHelper Documentation](https://joshclose.github.io/CsvHelper/)
- [Parsing CSV files in C#](https://www.codeproject.com/Articles/9258/A-Fast-CSV-Reader)
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - Standard du format CSV
