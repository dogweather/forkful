---
date: 2024-02-03 19:03:28.377238-07:00
description: "CSV (Comma-Separated Values) files are a common data exchange format\
  \ that represents tabular data in plain text, using commas to separate individual\u2026"
lastmod: '2024-03-13T22:45:00.109637-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma-Separated Values) files are a common data exchange format that\
  \ represents tabular data in plain text, using commas to separate individual\u2026"
title: Working with CSV
weight: 37
---

## What & Why?
CSV (Comma-Separated Values) files are a common data exchange format that represents tabular data in plain text, using commas to separate individual values. Programmers work with CSV files to import, export, and manipulate data with ease across various applications and services, as it is a simple, widely supported format compatible with spreadsheet applications, databases, and programming languages.

## How to:
Working with CSV files in C# can be accomplished through the `System.IO` namespace for basic operations, and for more complex manipulations or to handle larger files seamlessly, one might consider third-party libraries like `CsvHelper`. Below are examples of how to read from and write to CSV files using both approaches.

### Reading a CSV file using System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // Reading all the lines of the CSV file
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"First Column: {rowData[0]}, Second Column: {rowData[1]}");
        }
    }
}
```

**Sample output:**
```
First Column: Name, Second Column: Age
First Column: John Doe, Second Column: 30
```

### Writing to a CSV file using System.IO
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
        Console.WriteLine("CSV file written.");
    }
}
```

**Sample output:**
```
CSV file written.
```

### Using CsvHelper to Read CSV
To use CsvHelper, first, add the `CsvHelper` package to your project using NuGet Package Manager.

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
                Console.WriteLine($"First Column: {record.Name}, Second Column: {record.Age}");
            }
        }
    }
}
```

**Sample output:**
```
First Column: John Doe, Second Column: 30
First Column: Jane Smith, Second Column: 25
```

### Using CsvHelper to Write CSV
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
        
        Console.WriteLine("CSV file written with CsvHelper.");
    }
}
```

**Sample output:**
```
CSV file written with CsvHelper.
```
