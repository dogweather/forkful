---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:13:36.836303-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קודים במחשב רבים מצריכים את התאריך הנוכחי כדי לתעד אירועים, לעבד נתונים ולהציג אינפורמציה שוטפת למשתמשים. לכן, לדעת להשיג את התאריך והשעה הנוכחיים הוא כלי מרכזי בארסנל של כל מתכנת.

## איך לעשות:
בשפת C#, מחלקת `DateTime` בחבילת `System` היא זו שתשמש אותנו לצורך זה. כך אפשר להשיג את התאריך והשעה הנוכחיים:

```c#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```

פלט דוגמה:

```
2/28/2023 9:30:41 PM
```
לקבלת התאריך בלבד, השתמשו ב`DateTime.Today`.

```c#
Console.WriteLine(DateTime.Today);
```
פלט דוגמה:

```
2/28/2023
```

## סוגייה לעומק:
`DateTime` הוצגה ב-C# מגרסת 1.0. היא מתמקדת בתאריכים וזמנים בלוח הגרגוריאני. תאריכים בלוחות אחרים דורשים רכיבים נוספים.

ישנן אלטרנטיבות כמו `Stopwatch` למדידת פערי זמן ו`TimeZoneInfo` לעבודה עם אזורי זמן. גרסאות חדשות של C# מציעות את מחלקת `System.DateTimeOffset` שכוללת מידע על ההפרש מ-UTC, נתון שמועיל ביישומים מבוזרים וגלובליים.

בעבודה עם תאריכים, חשוב לדעת שמחלקת `DateTime` לא כוללת נתוני אזור זמן. זה יכול לגרום לבעיות אם לא מתייחסים לזה בישומים שמנהלים נתונים במסדר גודל גלובלי.

## לקרוא גם:
- [Microsoft Docs - DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [Microsoft Docs - DateTimeOffset](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=netcore-3.1)
- [Stack Overflow - When to use DateTime vs DateTimeOffset](https://stackoverflow.com/questions/4331189/when-to-use-datetime-vs-datetimeoffset)