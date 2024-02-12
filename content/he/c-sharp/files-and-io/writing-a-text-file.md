---
title:                "כתיבת קובץ טקסט"
aliases: - /he/c-sharp/writing-a-text-file.md
date:                  2024-02-03T19:28:06.133265-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת קובץ טקסט"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
