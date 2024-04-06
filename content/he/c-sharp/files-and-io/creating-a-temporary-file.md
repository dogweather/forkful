---
date: 2024-01-20 17:40:37.623023-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: (How to:) \u05DB\u05D3\
  \u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D1-C#, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05DE\u05D7\u05DC\u05E7\u05EA `Path` \u05D5`File` \u05DE\
  \u05D4-namespace `System.IO`."
lastmod: '2024-04-05T21:53:40.552744-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E7\u05D5\
  \u05D1\u05E5 \u05D6\u05DE\u05E0\u05D9 \u05D1-C#, \u05D0\u05EA\u05D4 \u05D9\u05DB\
  \u05D5\u05DC \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DE\u05D7\u05DC\u05E7\
  \u05EA `Path` \u05D5`File` \u05DE\u05D4-namespace `System.IO`."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

## איך לעשות: (How to:)
כדי ליצור קובץ זמני ב-C#, אתה יכול להשתמש במחלקת `Path` ו`File` מה-namespace `System.IO`:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // יצירת נתיב לקובץ זמני
        string tempFilePath = Path.GetTempFileName();

        // כתיבת טקסט לקובץ הזמני
        File.WriteAllText(tempFilePath, "Hello, Temp File!");

        // הדפסת נתונים מהקובץ הזמני
        Console.WriteLine(File.ReadAllText(tempFilePath));

        // מחיקת הקובץ בסיום השימוש
        File.Delete(tempFilePath);
    }
}
```
פלט לדוגמא:
```
Hello, Temp File!
```

## עיון מעמיק: (Deep Dive)
בעבר, קבצים זמניים היו חיוניים לניהול זיכרון במערכות עם משאבים מוגבלים. היום, הם עדיין שימושיים למקרים כמו שמירת נתונים גדולים במהלך עיבוד או כאשר ברצונך להימנע מזיהום מאגרי שיתוף. חלופה היא שימוש במנגנון זיכרון דינמי, כמו זיכרון מתועת (streaming) או מערכת מטמון. ברמה המימושית, ב-C# קיימת טיפול אוטומטי בקבצים זמניים, כולל יצירתם ומחיקתם באמצעות הכלים שסופקו ב-namespace `System.IO`.

## ראה גם: (See Also)
- [מחלקת Path](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netcore-3.1)
- [מחלקת File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [Namespace System.IO](https://docs.microsoft.com/en-us/dotnet/api/system.io?view=netcore-3.1)
- [ניהול זיכרון ב-C#](https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/)
