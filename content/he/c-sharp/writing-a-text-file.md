---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא תהליך שבו תוכנה שומרת נתונים בקובץ הניתן לקריאה בטקסט רגיל. תוכניתאים עושים זאת כדי לשמור הגדרות, לוגים, נתונים להעברה בין מערכות ועוד.

## איך לעשות:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\myfile.txt";
        string content = "שלום, זה קובץ טקסט נכתב בעברית.";

        File.WriteAllText(filePath, content);
        
        // בדוק שהקובץ נכתב
        if(File.Exists(filePath))
        {
            Console.WriteLine("קובץ נכתב בהצלחה.");
        }
    }
}
```
פלט לדוגמה:
```
קובץ נכתב בהצלחה.
```

## עיון נוסף
ניקוד לסטורי של כתיבת קבצים ב-C# מתחיל בגרסאות הראשונות של השפה ונתמך על ידי ספריית .NET Framework. ישנן אלטרנטיבות ל-`File.WriteAllText`, כולל `StreamWriter`, שמספק יותר גמישות בכתיבה לקובץ. בחינת הדרכים שונות לכתיבת קבצים יכולה להיות שימושית לבחירת הגישה הנכונה לפי צורכי התוכנית.

## לראות גם
- Microsoft Docs - `File.WriteAllText`: https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext
- מדריך ל-`StreamWriter`: https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter
- .NET API Browser: https://docs.microsoft.com/en-us/dotnet/api/
