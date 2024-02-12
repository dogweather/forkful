---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/c-sharp/using-regular-expressions.md
date:                  2024-02-03T19:17:11.423234-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) ב-C# הם כלי עוצמתי להתאמת תבניות בתוך מחרוזות, המאפשר למתכנתים לחפש, להחליף, לפצל או להוציא נתונים בצורה יעילה. המתכנתים משתמשים ב-regex למשימות הנעות מאימותים פשוטים, כמו בדיקת פורמט של דוא"ל, ועד למשימות מתקדמות של עיבוד טקסט בשל גמישותם וביצועיהם.

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
