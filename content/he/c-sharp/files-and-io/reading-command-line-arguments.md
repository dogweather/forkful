---
date: 2024-01-20 17:55:59.791801-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05EA \u05DC\
  \u05E7\u05D1\u05DC \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05D4\u05E0\u05D7\u05D9\
  \u05D5\u05EA \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E2\u05EA \u05E8\
  \u05D9\u05E6\u05EA\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D2\u05DE\u05D9\
  \u05E9\u05D5\u05EA \u05D5\u05DC\u05D0\u05E4\u05E9\u05E8 \u05E9\u05DC\u05D9\u05D8\
  \u05D4 \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\u05EA \u05D1\u05EA\u05D5\u05DB\u05E0\
  \u05D4."
lastmod: '2024-03-13T22:44:39.368544-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05EA \u05DC\
  \u05E7\u05D1\u05DC \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05D4\u05E0\u05D7\u05D9\
  \u05D5\u05EA \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E2\u05EA \u05E8\
  \u05D9\u05E6\u05EA\u05D4. \u05EA\u05DB\u05E0\u05D9\u05EA\u05D0\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4 \u05DC\u05D2\u05DE\u05D9\
  \u05E9\u05D5\u05EA \u05D5\u05DC\u05D0\u05E4\u05E9\u05E8 \u05E9\u05DC\u05D9\u05D8\
  \u05D4 \u05D3\u05D9\u05E0\u05D0\u05DE\u05D9\u05EA \u05D1\u05EA\u05D5\u05DB\u05E0\
  \u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה מאפשרת לתכנית לקבל מידע או הנחיות מהמשתמש בעת ריצתה. תכניתאים משתמשים בזה לגמישות ולאפשר שליטה דינאמית בתוכנה.

## איך לעשות:
```C#
// מקבל ארגומנטים משורת הפקודה
static void Main(string[] args)
{
    if(args.Length > 0)
    {
        Console.WriteLine("הנה הארגומנטים שקיבלת:");
        foreach (var arg in args)
        {
            Console.WriteLine(arg);
        }
    }
    else
    {
        Console.WriteLine("לא קיבלתי ארגומנטים כלל. תנסה שוב.");
    }
}
```

פלט לדוגמא (הפקודה הורצה עם ארגומנטים 'אחד', 'שתיים', 'שלוש'):
```
הנה הארגומנטים שקיבלת:
אחד
שתיים
שלוש
```

## טבילה עמוקה
בימים הראשונים, תכניות רצו משורת פקודה והיו תלויות בארגומנטים אלה לשליטה בהתנהגות. יש גם אלטרנטיבות, כמו קריאת קבצי קונפיגורציה או ממשק משתמש גרפי, אבל ארגומנטים משורת הפקודה עדיין נפוצים בגלל פשטות ואוטומציה. ב-C# גרסה 8.0 ומעלה, ניתן גם להשתמש ב Top-Level Statements כדי לפשט את הקוד עוד יותר.

## ראו גם:
- [Documentation for Command Line Arguments in .NET](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [Top-Level Statements in C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/top-level-statements)
- [Using Environment.GetCommandLineArgs()](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netframework-4.8)
