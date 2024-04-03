---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:40.963830-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E32\u0E23\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\u0E1F\u0E25\u0E4C CSV \u0E43\u0E19\
  \ C# \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\u0E33\u0E44\u0E14\u0E49\u0E1C\u0E48\
  \u0E32\u0E19\u0E40\u0E19\u0E21\u0E2A\u0E40\u0E1B\u0E0B `System.IO` \u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\
  \u0E23\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.248366-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV \u0E43\u0E19 C# \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E17\
  \u0E33\u0E44\u0E14\u0E49\u0E1C\u0E48\u0E32\u0E19\u0E40\u0E19\u0E21\u0E2A\u0E40\u0E1B\
  \u0E0B `System.IO` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E33\
  \u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\
  \ \u0E41\u0E25\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\
  \u0E19\u0E02\u0E36\u0E49\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\
  \u0E23\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E19\u0E32\u0E14\u0E43\u0E2B\u0E0D\u0E48\u0E44\
  \u0E14\u0E49\u0E23\u0E32\u0E1A\u0E23\u0E37\u0E48\u0E19 \u0E2D\u0E32\u0E08\u0E1E\u0E34\
  \u0E08\u0E32\u0E23\u0E13\u0E32\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\
  \u0E35\u0E02\u0E2D\u0E07\u0E1D\u0E48\u0E32\u0E22\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \ \u0E40\u0E0A\u0E48\u0E19 `CsvHelper` \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\
  \u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E02\
  \u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E08\
  \u0E32\u0E01 \u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E25\u0E07\u0E44\u0E1F\
  \u0E25\u0E4C CSV \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E17\u0E31\u0E49\u0E07\u0E2A\
  \u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E19\u0E35\u0E49\n\n#."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## วิธีการ:
การทำงานกับไฟล์ CSV ใน C# สามารถทำได้ผ่านเนมสเปซ `System.IO` สำหรับการดำเนินการพื้นฐาน และสำหรับการจัดการที่ซับซ้อนขึ้นหรือจัดการไฟล์ขนาดใหญ่ได้ราบรื่น อาจพิจารณาใช้ไลบรารีของฝ่ายที่สาม เช่น `CsvHelper` ด้านล่างเป็นตัวอย่างของวิธีการอ่านจาก และเขียนลงไฟล์ CSV โดยใช้ทั้งสองวิธีนี้

### อ่านไฟล์ CSV โดยใช้ System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // อ่านทุกบรรทัดของไฟล์ CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"คอลัมน์แรก: {rowData[0]}, คอลัมน์ที่สอง: {rowData[1]}");
        }
    }
}
```

**ผลลัพธ์ตัวอย่าง:**
```
คอลัมน์แรก: ชื่อ, คอลัมน์ที่สอง: อายุ
คอลัมน์แรก: John Doe, คอลัมน์ที่สอง: 30
```

### เขียนเข้าไฟล์ CSV โดยใช้ System.IO
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
            "ชื่อ,อายุ",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("เขียนไฟล์ CSV แล้ว.");
    }
}
```

**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์ CSV แล้ว.
```

### ใช้ CsvHelper อ่าน CSV
สำหรับใช้ CsvHelper เริ่มต้นด้วยการเพิ่มแพ็กเกจ `CsvHelper` ลงในโปรเจ็กต์ของคุณโดยใช้ NuGet Package Manager.

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
                Console.WriteLine($"คอลัมน์แรก: {record.Name}, คอลัมน์ที่สอง: {record.Age}");
            }
        }
    }
}
```

**ผลลัพธ์ตัวอย่าง:**
```
คอลัมน์แรก: John Doe, คอลัมน์ที่สอง: 30
คอลัมน์แรก: Jane Smith, คอลัมน์ที่สอง: 25
```

### ใช้ CsvHelper เขียน CSV
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
        public string ชื่อ { get; set; }
        public int อายุ { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { ชื่อ = "John Doe", อายุ = 30 },
            new Person { ชื่อ = "Jane Smith", อายุ = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("เขียนไฟล์ CSV ด้วย CsvHelper.");
    }
}
```

**ผลลัพธ์ตัวอย่าง:**
```
เขียนไฟล์ CSV ด้วย CsvHelper.
```
