---
date: 2024-01-20 17:32:38.797999-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E2\u05D3 \u05DC\
  -C# 2.0, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D4\u05D9\u05D9\u05EA\u05D4 \u05DE\u05D5\u05E8\u05DB\u05D1\u05EA \u05D9\
  \u05D5\u05EA\u05E8 \u2013 \u05D4\u05D9\u05D9\u05E0\u05D5 \u05E6\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05DB\u05DC \u05D7\u05DC\u05E7\
  \ \u05D1\u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E0\u05E4\u05E8\u05D3. \u05E2\u05DD\
  \ \u05D4\u05E7\u05D3\u05DE\u05EA \u05D4\u05DE\u05D7\u05DC\u05E7\u05D4 `DateTime`,\
  \ \u05DB\u05DC \u05D6\u05D4 \u05D4\u05E4\u05DA \u05DC\u05E4\u05E9\u05D5\u05D8 \u05D1\
  \u05E8\u05DE\u05D5\u05EA.\u2026"
lastmod: '2024-04-05T21:53:40.543894-06:00'
model: gpt-4-1106-preview
summary: "\u05E2\u05D3 \u05DC-C# 2.0, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D4\u05D9\u05D9\u05EA\u05D4 \u05DE\u05D5\u05E8\
  \u05DB\u05D1\u05EA \u05D9\u05D5\u05EA\u05E8 \u2013 \u05D4\u05D9\u05D9\u05E0\u05D5\
  \ \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05D4\u05E9\u05D5\u05D5\u05EA \u05DB\
  \u05DC \u05D7\u05DC\u05E7 \u05D1\u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E0\u05E4\
  \u05E8\u05D3."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05E0\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
```C#
using System;

public class DateComparison
{
    static void Main()
    {
        DateTime date1 = new DateTime(2023, 3, 15); // 15 מרץ 2023
        DateTime date2 = DateTime.Now; // התאריך והשעה הנוכחית
        
        int comparison = DateTime.Compare(date1, date2);

        if (comparison < 0)
        {
            Console.WriteLine("date1 is earlier than date2.");
        }
        else if (comparison == 0)
        {
            Console.WriteLine("date1 is the same as date2.");
        }
        else
        {
            Console.WriteLine("date1 is later than date2.");
        }
    }
}

// פלט משתנה בהתאם לתאריך ושעה כאשר הקוד רץ
```

## צלילה לעומק:
עד ל-C# 2.0, השוואת תאריכים הייתה מורכבת יותר – היינו צריכים להשוות כל חלק בתאריך בנפרד. עם הקדמת המחלקה `DateTime`, כל זה הפך לפשוט ברמות. כאשר משווים תאריכים, זכרו שזמנים גם מושפעים מאזורי זמן ושעון קיץ – `DateTimeOffset` יכול לעזור פה.

חלופות? תכניתנים יכולים להשתמש ב-`TimeSpan` כדי לחשב הבדלים ולבצע השוואות, או במחלקת `CompareTo` של `DateTime` להשוואות מדוייקות יותר. אלו אומנם פתרונות אפשריים, אך הפונקציה `DateTime.Compare()` היא הפשוטה והמהירה ביותר לרוב הצרכים.

## ראו גם:
- MSDN Documentation on [DateTime.Compare](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-6.0)
- Tutorial on [DateTime and DateTimeOffset in C#](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- Stack Overflow discussion on [DateTime vs DateTimeOffset](https://stackoverflow.com/questions/4331189/datetime-vs-datetimeoffset)
