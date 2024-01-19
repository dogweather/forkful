---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה זה & למה?
קריאת פרמטרים מהשורה של הפקודה היא דרך לקבל קלט מהמשתמש כאשר הוא מריץ את האפליקציה. מתכנתים משתמשים בזה כדי ליצור אפליקציות גמישות ותורמת לאפיקות עיבוד מניחות.

## איך:
הנה דוגמא של קריאת ארגומנטים משורת הפקודה פשוטה ב-C#:

```C
class Program
{
    static void Main(string[] args)
    {
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument[{i}] = {args[i]}");
        }
    }
}
```
אם נרוץ את הפקודה `dotnet run arg1 arg2 arg3`, שורת הפלט תהיה
```
Argument[0] = arg1
Argument[1] = arg2
Argument[2] = arg3
```

## בעומק:
קריאת פרמטרים מהשורה של הפקודה היא תהליך שהתפתח עם השפה של C, דרך CLR (Common Language Runtime). בC#, תזרים זה משתמש בטיפוס נתונים string[], אבל בשפות פרודיה אחרת יכולות להיות בחירות שונות.
חלופה נוספת לקריאה של ארגומנטים משורת הפקודה היא שימוש בספריית קוד פתוח כמו CommandLineParser. ספריות אלה מעניקות גמישות נוספת בטיפול בארגומנטים.
במהלך הריצה, יד ה-CLR ב-C# מועברת אל "Main" כאשר האפליקציה מתחילה, וכל הארגומנטים משורת הפקודה מועברים כארגומנטים ל- "Main".

## ראה גם:
- [קריאת ארגומנטים משורת הפקודה ב-C# Microsoft Docs](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [CommandLineParser GitHub](https://github.com/commandlineparser/commandline)
- [Common Language Runtime (CLR) Microsoft Docs](https://docs.microsoft.com/he-il/dotnet/standard/clr)