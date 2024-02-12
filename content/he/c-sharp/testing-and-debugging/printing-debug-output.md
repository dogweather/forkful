---
title:                "הדפסת פלט לניפוי באגים"
aliases: - /he/c-sharp/printing-debug-output.md
date:                  2024-01-20T17:52:26.700829-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט לניפוי באגים היא שימוש בפקודות להצגת מידע במהלך ריצת התוכנית. מתכנתים עושים זאת כדי לאתר בעיות ולהבין איך התוכנה פועלת 'תחת המכסה'.

## איך לעשות:
```C#
using System;

class Program
{
    static void Main()
    {
        // פשוט להדפיס לקונסול
        Console.WriteLine("ניפוי באגים בפעולה");

        // הדפסת ערכים משתנים
        int x = 5;
        int y = 3;
        Console.WriteLine($"התוצאה היא: {x + y}");
    }
}
```
פלט דוגמה:
```
ניפוי באגים בפעולה
התוצאה היא: 8
```

## צלילה לעומק
למרות שהדפסה לקונסול היא מושג יסודי, כלים יותר מתקדמים כמו מנגנוני לוגינג (logging) התפתחו לאורך השנים. במקום `Console.WriteLine()`, יש אנשים שמשתמשים ב־`Debug.WriteLine()` עבור .NET, אשר מאפשר להדפיס פלט רק בעת כשהתוכנית במצב ניפוי באגים. קיימים גם כלים חיצוניים כמו log4net או NLog שמספקים יכולות לוגינג עוצמתיות ומורכבות.

## לקריאה נוספת
- מידע נוסף על ניפוי באגים ב-C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/testing-and-debugging/
- מדריך על מערכת הלוגינג ב-.NET: https://docs.microsoft.com/en-us/dotnet/core/extensions/logging
- רשימת כלים לניפוי באגים ב-C#: https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-absolute-beginners?view=vs-2022
