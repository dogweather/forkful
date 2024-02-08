---
title:                "עובדים עם CSV"
aliases:
- he/c-sharp/working-with-csv.md
date:                  2024-02-03T19:19:51.212513-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
קבצי CSV (ערכים מופרדים בפסיק) הם פורמט נפוץ להחלפת נתונים שמייצגים נתונים טבלאיים בטקסט רגיל, תוך שימוש בפסיקים להפרדת הערכים היחידים. מתכנתים עוסקים בקבצי CSV כדי לייבא, לייצא, ולעבד נתונים בקלות בין שירותים ואפליקציות שונות, מאחר שמדובר בפורמט פשוט, נתמך ברחבי, התואם לאפליקציות נתונים טבלאיים, מסדי נתונים, ושפות תכנות.

## איך לעשות:
עבודה עם קבצי CSV ב-C# יכולה להתבצע דרך המרחב השם `System.IO` לפעולות בסיסיות, ולמניפולציות מורכבות יותר או לטיפול בקבצים גדולים ללא תקלות, כדאי לשקול להשתמש בספריות צד שלישי כמו `CsvHelper`. להלן דוגמאות לקריאה מקבצי CSV וכתיבה אליהם בשתי הדרכים.

### קריאת קובץ CSV באמצעות System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // קריאת כל השורות של קובץ ה-CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"עמודה ראשונה: {rowData[0]}, עמודה שנייה: {rowData[1]}");
        }
    }
}
```

**פלט לדוגמה:**
```
עמודה ראשונה: שם, עמודה שנייה: גיל
עמודה ראשונה: ג'ון דו, עמודה שנייה: 30
```

### כתיבה לקובץ CSV באמצעות System.IO
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
            "שם,גיל",
            "ג'ון דו,30",
            "ג'יין סמית',25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("קובץ CSV נכתב.");
    }
}
```

**פלט לדוגמה:**
```
קובץ CSV נכתב.
```

### שימוש ב-CsvHelper לקריאת CSV
לשימוש ב-CsvHelper, ראשית, הוסף את חבילת `CsvHelper` לפרויקט שלך באמצעות ניהול חבילות NuGet.

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
                Console.WriteLine($"עמודה ראשונה: {record.Name}, עמודה שנייה: {record.Age}");
            }
        }
    }
}
```

**פלט לדוגמה:**
```
עמודה ראשונה: ג'ון דו, עמודה שנייה: 30
עמודה ראשונה: ג'יין סמית', עמודה שנייה: 25
```

### שימוש ב-CsvHelper לכתיבת CSV
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
            new Person { Name = "ג'ון דו", Age = 30 },
            new Person { Name = "ג'יין סמית'", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("קובץ CSV נכתב עם CsvHelper.");
    }
}
```

**פלט לדוגמה:**
```
קובץ CSV נכתב עם CsvHelper.
```
