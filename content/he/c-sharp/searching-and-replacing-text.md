---
title:                "חיפוש והחלפת טקסט"
aliases:
- he/c-sharp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:53.694860-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט ב-C# זה פשוט מציאת רצפים של תווים והחלפתם באחרים. מתכנתים עושים את זה לעיבוד נתונים, ניקוי מידע, ולשמירה על קוד נקי ויעיל.

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
