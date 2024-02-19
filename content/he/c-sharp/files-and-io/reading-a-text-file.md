---
aliases:
- /he/c-sharp/reading-a-text-file/
date: 2024-01-20 17:54:36.406539-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-C# \u05D4\u05D9\u05D0 \u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05D8\u05D5\u05E2\u05E0\u05D9\
  \u05DD \u05EA\u05D5\u05DB\u05DF \u05DE\u05EA\u05D5\u05DA \u05E7\u05D5\u05D1\u05E5\
  \ \u05D8\u05E7\u05E1\u05D8 \u05E7\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA \u05D4\
  \u05EA\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05E0\u05D5. \u05D6\u05D4 \u05D7\u05E9\
  \u05D5\u05D1 \u05DB\u05D9 \u05D6\u05D4 \u05D0\u05D5\u05E4\u05DF \u05D4\u05DB\u05E8\
  \u05D7\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05DE\u05D9\u05D3\u05E2\
  \ \u05E9\u05E0\u05E9\u05DE\u05E8 \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA, \u05DC\
  \u05EA\u05E4\u05E2\u05D5\u05DC\u2026"
lastmod: 2024-02-18 23:08:52.860701
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-C# \u05D4\u05D9\u05D0 \u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D5 \u05D8\u05D5\u05E2\u05E0\u05D9\u05DD\
  \ \u05EA\u05D5\u05DB\u05DF \u05DE\u05EA\u05D5\u05DA \u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05E7\u05D9\u05D9\u05DD \u05DC\u05EA\u05D5\u05DA \u05D4\u05EA\
  \u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05E0\u05D5. \u05D6\u05D4 \u05D7\u05E9\u05D5\
  \u05D1 \u05DB\u05D9 \u05D6\u05D4 \u05D0\u05D5\u05E4\u05DF \u05D4\u05DB\u05E8\u05D7\
  \u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05DE\u05D9\u05D3\u05E2 \u05E9\
  \u05E0\u05E9\u05DE\u05E8 \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA, \u05DC\u05EA\
  \u05E4\u05E2\u05D5\u05DC\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-C# היא לבצע פעולה שבה אנו טוענים תוכן מתוך קובץ טקסט קיים לתוך התכנית שלנו. זה חשוב כי זה אופן הכרחי לעבוד עם מידע שנשמר חיצונית, לתפעול וניתוח.

## איך לעשות:
קוד הדוגמה למטה מראה כיצד לקרוא תוכן מקובץ טקסט לתוך מחרוזת.

```C#
using System;
using System.IO;

class ReadTextFileExample
{
    static void Main()
    {
        string filePath = @"C:\example\myfile.txt"; // הקובץ לקריאה
        if (File.Exists(filePath))
        {
            string content = File.ReadAllText(filePath);
            Console.WriteLine(content);
        }
        else
        {
            Console.WriteLine("File not found.");
        }
    }
}
```
פלט לדוגמה:
```
Hello, this is the content of the text file.
```

## עיון מעמיק:
בעבר, קריאת קבצים הייתה תהליך מסובך יותר שדרש הבנה של זרימת ביינרית וניהול משאבים. עם הזמן וההתפתחות של השפות, התהליך פשט והפך לידידותי יותר למתכנתים.

קיימות אלטרנטיבות נוספות לתיבת הקריאה `File.ReadAllText` שנמצאת למעלה, כמו לדוגמא `File.ReadAllLines` שקורא כל שורה אל תוך מערך של מחרוזות או `StreamReader` שמאפשר קריאה בזרימה וביצועית יותר לקבצים גדולים.

בתוך הקוד, כאשר אנו עובדים עם שיטת `File.ReadAllText` או שיטות דומות, העצלים המובנות של C# מטפלות בפתיחת הקובץ, קריאת התוכן וסגירת הקובץ לאחר השימוש, מה שהופך את הקוד לנקי ובטוח יותר תוך מניעת זליגת משאבים ושגיאות.

## גם כדאי לראות:
- Microsoft Docs על קריאת קבצים: [איך לקרוא טקסט מקבצים](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file)
- אתר CodeProject על עבודה עם קבצים ב-C#: [C# File I/O Operations](https://www.codeproject.com/Articles/415732/Csharp-File-I-O-Operations)
- Stack Overflow שאלות ותשובות בנושא קריאת ועיבוד קבצי טקסט: [Stack Overflow - Reading Text Files](https://stackoverflow.com/questions/tagged/c%23+file-io+text-files)
