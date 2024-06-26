---
date: 2024-01-20 17:52:26.700829-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05DE\u05E8\
  \u05D5\u05EA \u05E9\u05D4\u05D3\u05E4\u05E1\u05D4 \u05DC\u05E7\u05D5\u05E0\u05E1\
  \u05D5\u05DC \u05D4\u05D9\u05D0 \u05DE\u05D5\u05E9\u05D2 \u05D9\u05E1\u05D5\u05D3\
  \u05D9, \u05DB\u05DC\u05D9\u05DD \u05D9\u05D5\u05EA\u05E8 \u05DE\u05EA\u05E7\u05D3\
  \u05DE\u05D9\u05DD \u05DB\u05DE\u05D5 \u05DE\u05E0\u05D2\u05E0\u05D5\u05E0\u05D9\
  \ \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 (logging) \u05D4\u05EA\u05E4\u05EA\u05D7\u05D5\
  \ \u05DC\u05D0\u05D5\u05E8\u05DA \u05D4\u05E9\u05E0\u05D9\u05DD. \u05D1\u05DE\u05E7\
  \u05D5\u05DD `Console.WriteLine()`, \u05D9\u05E9 \u05D0\u05E0\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-04-05T22:50:53.525009-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05DE\u05E8\u05D5\u05EA \u05E9\u05D4\u05D3\u05E4\u05E1\u05D4 \u05DC\
  \u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D4\u05D9\u05D0 \u05DE\u05D5\u05E9\u05D2\
  \ \u05D9\u05E1\u05D5\u05D3\u05D9, \u05DB\u05DC\u05D9\u05DD \u05D9\u05D5\u05EA\u05E8\
  \ \u05DE\u05EA\u05E7\u05D3\u05DE\u05D9\u05DD \u05DB\u05DE\u05D5 \u05DE\u05E0\u05D2\
  \u05E0\u05D5\u05E0\u05D9 \u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 (logging) \u05D4\u05EA\
  \u05E4\u05EA\u05D7\u05D5 \u05DC\u05D0\u05D5\u05E8\u05DA \u05D4\u05E9\u05E0\u05D9\
  \u05DD."
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

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
