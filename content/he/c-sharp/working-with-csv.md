---
title:                "C#: עובדים עם קבצי csv"
simple_title:         "עובדים עם קבצי csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# מדוע

CSV היא תבנית נתונים נפוצה ונוחה לשימוש עבור מתכנתים. בעזרת CSV, ניתן לאחסן נתונים בפורמט טקסט פשוט ולעבד אותם בקלות עבור יישומים שונים. המאמר הזה ילמד אותך כיצד לעבוד עם CSV בשפת תכנות C#.

# איך לעבוד עם CSV ב-C#

בשפת תכנות C#, ניתן לעבוד עם קבצי CSV באמצעות מספר מתודות וספריות קיימות. הנה דוגמאות לשימוש ב-C# לטעינת נתונים מקובץ CSV והדפסתם בקונסולה:

```C#
using System;
using System.IO;
using System.Linq;

namespace CSVExample
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] lines = File.ReadAllLines("example.csv"); // קריאת כל השורות בקובץ
            foreach (string line in lines.Skip(1)) // ביחידות הידועות בשם תיך, עבור כל השורות ללא הכותרות
            {
                string[] values = line.Split(','); // פיצול השורה לערכים מופרדים בפסיק
                Console.WriteLine($"{values[0]} עודכן ל-{values[1]}"); // הדפסת הערכים
            }
        }
    }
}

```

# מעמקים בעבודה עם CSV

השימוש בספריות כמו `CsvHelper` ו-`TextFieldParser` יכול לשפר את הפעולה עם CSV ב-C#. ניתן גם ליצור מבני נתונים (data classes) כדי לטעון ולהכין נתונים מקובץ CSV בצורה יעילה יותר. כדאי גם ללמוד על פקודות כמו `link` לניתוח מתוחכם של נתונים ב-C#.

# ראה גם

- [טיפים נוספים עבור עבודה עם CSV ב-C#](https://code-maze.com/working-csv-files-csharp/)
- [התיקייה הרשמית של Microsoft עם דוגמאות לעבודה עם CSV ב-C#](https://github.com/microsoft/CsvHelper)