---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-Separated Values) זה פורמט קובץ טקסט פשוט שמשמש לאחסון נתונים. מתכנתים עובדים איתו כי הוא קריא, נגיש וקל לעיבוד.

## How to:
קריאה מ-CSV:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        var filePath = @"path\to\your\file.csv";
        var lines = File.ReadAllLines(filePath);

        foreach (var line in lines)
        {
            var values = line.Split(',');
            Console.WriteLine($"{values[0]} {values[1]}");
        }
    }
}
```
פלט דוגמא:
```
שם ראשון שם אחרון
ישראל ישראלי
```

כתיבה ל-CSV:
```C#
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var data = new List<string[]>
        {
            new string[] {"שם ראשון", "שם אחרון"},
            new string[] {"ישראל", "ישראלי"}
        };
        
        var filePath = @"path\to\new\file.csv";
        
        using (var sw = new StreamWriter(filePath))
        {
            foreach (var line in data)
            {
                sw.WriteLine(string.Join(",", line));
            }
        }
    }
}
```

## Deep Dive:
CSV הוא פורמט עתיק שנוצר בשנות ה-70. הוא בר-שימוש גבוה גם היום בזכות פשטותו. ישנם חלופות כמו XML ו-JSON, אבל ל-CSV יתרונות במהירות ופשטות. פרסר CSV מובנה אינו קיים ב-C#, אבל ניתן להשתמש בפונקציות קריאה וכתיבה סטנדרטיות או להשתמש בספריות צד שלישי.

## See Also:
- [Microsoft Documentation on File I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [CsvHelper library](https://joshclose.github.io/CsvHelper/)
- [RFC 4180 - Common Format and MIME Type for CSV Files](https://tools.ietf.org/html/rfc4180)
