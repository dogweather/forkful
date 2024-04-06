---
date: 2024-01-20 17:57:53.694860-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D7\u05D9\
  \u05E4\u05D5\u05E9 \u05D5\u05D4\u05D4\u05D7\u05DC\u05E4\u05D4 \u05E9\u05DC \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-C# \u05DE\u05D1\u05D5\u05E1\u05E1\u05D9\u05DD \u05E2\u05DC\
  \ \u05DE\u05EA\u05D5\u05D3\u05D5\u05EA \u05DE\u05D7\u05DC\u05E7\u05EA String. \u05D4\
  \u05D9\u05E1\u05D8\u05D5\u05E8\u05D9\u05EA, \u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\
  \u05E0\u05D5\u05EA \u05D4\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1\u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05EA\u05DE\u05EA\u05D9\u05D5\u05EA \u05D5\
  \u05EA\u05D5\u05D5\u05D9 \u05E4\u05E7\u05D5\u05D3\u05D4 \u05DC\u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8, \u05D0\u05D1\u05DC C# \u05DE\u05E0\u05D2\u05D9\
  \u05E9\u2026"
lastmod: '2024-04-05T22:50:53.500790-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D4\u05D7\u05DC\u05E4\
  \u05D4 \u05E9\u05DC \u05D8\u05E7\u05E1\u05D8 \u05D1-C# \u05DE\u05D1\u05D5\u05E1\u05E1\
  \u05D9\u05DD \u05E2\u05DC \u05DE\u05EA\u05D5\u05D3\u05D5\u05EA \u05DE\u05D7\u05DC\
  \u05E7\u05EA String."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## איך לעשות:
קחו את הדוגמה הזו:

```c#
using System;

class Program
{
    static void Main()
    {
        string originalText = "שלום עולם! ברוכים הבאים ל-C#.";
        string modifiedText = originalText.Replace("עולם", "מתכנתים");

        Console.WriteLine(modifiedText); // יכתוב "שלום מתכנתים! ברוכים הבאים ל-C#."
    }
}
```
קוד פשוט זה מוצא את המילה "עולם" ומחליף אותה ב"מתכנתים".

## טבילה עמוקה
החיפוש וההחלפה של טקסט ב-C# מבוססים על מתודות מחלקת String. היסטורית, שפות תכנות השתמשו בפונקציות מתמתיות ותווי פקודה לעיבוד טקסט, אבל C# מנגיש את הפונקציות הללו דרך מתודות ברורות ונוחות לשימוש.

אלטרנטיבות? אפשר להשתמש גם בביטויים רגולריים (Regular Expressions) לחיפוש והחלפה מורכבת יותר.

```c#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalText = "שלום עולם! ברוכים הבאים ל-C#. דוגמה: 123-45-6789";
        string pattern = @"\d{3}-\d{2}-\d{4}";
        string replacement = "XXX-XX-XXXX";

        string result = Regex.Replace(originalText, pattern, replacement);

        Console.WriteLine(result); // ידפיס "שלום עולם! ברוכים הבאים ל-C#. דוגמה: XXX-XX-XXXX"
    }
}
```

## הנה גם
- מדריכים על ביטויים רגולריים (Regular Expressions) ב-C#: [Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)

- המחלקה `String` במיקרוסופט דוקומנטציה: [Microsoft Docs - String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string)

- בית הספר ל-C# של מיקרוסופט (מכיל הדרכות שימושיות): [Microsoft C# Guide](https://docs.microsoft.com/en-us/dotnet/csharp/)
