---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:11.423234-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\
  \u05E7 \u05D0\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05DB\u05D9\u05DC\
  \u05D4 \u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E9\u05D9\u05D8\
  \u05EA `Regex.IsMatch` \u05DE\u05EA\u05D5\u05DA \u05D4\u05DE\u05E8\u05D7\u05D1 \u05D4\
  \u05E9\u05DE\u05D5\u05EA `System.Text.RegularExpressions`."
lastmod: '2024-03-13T22:44:39.326169-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05DD \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DE\u05DB\u05D9\u05DC\u05D4 \u05EA\u05D1\u05E0\u05D9\
  \u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\
  \u05E9\u05EA\u05DE\u05E9 \u05D1\u05E9\u05D9\u05D8\u05EA `Regex.IsMatch` \u05DE\u05EA\
  \u05D5\u05DA \u05D4\u05DE\u05E8\u05D7\u05D1 \u05D4\u05E9\u05DE\u05D5\u05EA `System.Text.RegularExpressions`."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך ל:


### התאמת תבנית פשוטה
כדי לבדוק אם מחרוזת מכילה תבנית מסוימת, ניתן להשתמש בשיטת `Regex.IsMatch` מתוך המרחב השמות `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // פלט: True
    }
}
```

### חילוץ נתונים
ניתן לחלץ נתונים ממחרוזת באמצעות קבוצות ב-regex באמצעות שיטת `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // פלט: שנה: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // פלט: חודש: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // פלט: יום: 12
        }
    }
}
```

### החלפת טקסט
שיטת `Regex.Replace` מאפשרת להחליף טקסט במחרוזת שמתאימה לתבנית מסוימת.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // פלט: Visit Google!
    }
}
```

### פיצול מחרוזות
ניתן לפצל מחרוזת למערך בתלות בתבנית regex באמצעות שיטת `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // פלט: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### שימוש בספריות צד שלישי
למרות שמסגרת .NET מספקת תמיכה נרחבת בביטויים רגולריים, ישנן גם ספריות של צד שלישי כמו `PCRE.NET` המציעות ביטויים רגולריים תואמים ל-Perl (PCRE) ב-C#. זה יכול להיות שימושי אם אתה זקוק לתכונות או לתחביר ממנוע ה-regex של Perl שאינם זמינים במימוש של .NET.

כדי להשתמש ב-`PCRE.NET`, ראשית תצטרך להתקין את חבילת ה-NuGet שלה, ואז תוכל להשתמש בה באופן דומה לכיצד אתה משתמש במחלקות regex המקוריות של .NET.

```csharp
// דוגמה לשימוש ב-PCRE.NET כאן
// הערה: דמיינו דוגמה דומה לזו שלמעלה, מותאמת להדגמת תכונה ייחודית ל-PCRE.NET.
```

כשמשתלבים ספריות צד שלישי לביטויים רגולריים, תמיד ייעץ לעיין בתיעוד שלהם למידע פרטני על השימוש והתאימות.
