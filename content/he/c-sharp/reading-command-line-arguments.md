---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- he/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:59.791801-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/reading-command-line-arguments.md"
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
