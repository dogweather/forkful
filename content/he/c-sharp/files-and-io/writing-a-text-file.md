---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:06.133265-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-C# \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05D4\
  \ \u05D0\u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\
  \u05D9 - \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E2\
  \u05D1\u05D5\u05E8 \u05DE\u05D2\u05D5\u05D5\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD, \u05DB\u05DE\u05D5 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05D9\u05D9\u05E6\
  \u05D5\u05D0 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05E0\u05D9\u05D4\
  \u05D5\u05DC\u2026"
lastmod: '2024-03-11T00:14:12.826483-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-C# \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05D4 \u05D0\
  \u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E9\u05DC \u05E7\u05D1\u05E6\u05D9 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\
  \ - \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E2\u05D1\
  \u05D5\u05E8 \u05DE\u05D2\u05D5\u05D5\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\
  \u05DD, \u05DB\u05DE\u05D5 \u05EA\u05D9\u05E2\u05D5\u05D3, \u05D9\u05D9\u05E6\u05D5\
  \u05D0 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05D0\u05D5 \u05E0\u05D9\u05D4\u05D5\
  \u05DC\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-C# כוללת יצירה או שינוי של קבצי טקסט במערכת הקבצים באופן תכנותי - משימה יסודית עבור מגוון יישומים, כמו תיעוד, ייצוא נתונים, או ניהול תצורה. מתכנתים ביצעו פעולה זו כדי לשמר נתונים בין הפעלות, לשתף מידע בין מערכות, או לאחסן פלטים הקריאים לאדם.

## איך לעשות זאת:
C# מפשטת פעולות עם קבצים באמצעות המרחב השם `System.IO` שלה, ומספקת דרכים ישרות לכתוב קבצי טקסט. הנה איך לכתוב קובץ טקסט בסיסי ולהוסיף טקסט לקובץ קיים.

### כתיבה לקובץ טקסט מאפס
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "שלום, עולם!";

        // כתוב את התוכן לקובץ חדש
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("הקובץ נכתב בהצלחה.");
    }
}
```
**פלט לדוגמא:**
```
הקובץ נכתב בהצלחה.
```

### הוספת טקסט לקובץ קיים
אם ברצונך להוסיף טקסט לסוף קובץ קיים, תוכל להשתמש בשיטת `File.AppendAllText`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nמוסיף תוכן נוסף.";

        // צרף תוכן לקובץ
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("התוכן צורף בהצלחה.");
    }
}
```
**פלט לדוגמא:**
```
התוכן צורף בהצלחה.
```

### שימוש בספריות צד שלישי: `StreamWriter`
לשליטה מדוקדקת יותר על הכתיבה, כולל שטיפה אוטומטית ובחירת קידוד, השתמש ב`StreamWriter`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "זוהי דוגמה באמצעות StreamWriter.";

        // שימוש ב-StreamWriter לכתיבה לקובץ
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("הקובץ נכתב בהצלחה עם StreamWriter.");
    }
}
```
**פלט לדוגמא:**
```
הקובץ נכתב בהצלחה עם StreamWriter.
```

כל אחת מהשיטות האלה משרתת צרכים שונים: שיטות ישירות של `File` לפעולות מהירות, ו-`StreamWriter` לסצנריות כתיבה מורכבות יותר. בחר בהתאם לדרישות הספציפיות שלך, בהתחשב בגורמים כמו ביצועים וגודל הקובץ.
