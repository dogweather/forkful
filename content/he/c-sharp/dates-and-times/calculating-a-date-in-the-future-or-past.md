---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- /he/c-sharp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:23.197951-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
לחשב תאריך בעתיד או בעבר זו בסך הכל יכולת לקבוע מתי משהו יקרה או קרה, מתוך נקודת זמן מסוימת. תוכניתנים עושים את זה לתכנון אירועים, לעקוב אחר מועדים, או לפענח נתונים תקופתיים.

## איך לעשות:
פשוט קח את הקוד הבא והשתמש בו:

```C#
using System;

public class DateExample
{
    public static void Main()
    {
        DateTime today = DateTime.Now;
        DateTime futureDate = today.AddDays(10);//עשרה ימים מהיום
        DateTime pastDate = today.AddDays(-10);//עשרה ימים לפני
        Console.WriteLine("Today: " + today.ToString("dd/MM/yyyy"));
        Console.WriteLine("Date in the future: " + futureDate.ToString("dd/MM/yyyy"));
        Console.WriteLine("Date in the past: " + pastDate.ToString("dd/MM/yyyy"));
    }
}
```
וזה תוצאות הדוגמה:

```
Today: 14/03/2023
Date in the future: 24/03/2023
Date in the past: 04/03/2023
```

## נפנוף לעומק:
לחישוב תאריכים יש חשיבות היסטורית רמה - מלוח השנה יוליאני לגרגוריאני. המעברים בין השיטות שינו תאריכים והפכו חישובים למורכבים יותר. ב-C#, `DateTime` הוא המחלקה שמספקת התמיכה בתאריכים ובזמנים. חלופות כוללות `DateTimeOffset` עבור התמכרות לאזורי זמן ו-`TimeSpan` להבדלי זמן. ברמת המימוש, `DateTime` עוקף בעיות של שינוי שעת קיץ ועוד עבור חישובים פשוטים כמו הדוגמה. לפעמים יש צורך להפעיל חשבון שנים עבריות ולזה יש ספריות מיוחדות.

## לראות גם:
- מידע נוסף על `DateTime`: [Microsoft Documentation - DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- מידע על `TimeSpan`: [Microsoft Documentation - TimeSpan](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-6.0)
- למידע על עבודה עם אזורי זמן מומלץ `TimeZoneInfo`: [Microsoft Documentation - TimeZoneInfo](https://docs.microsoft.com/en-us/dotnet/api/system.timezoneinfo?view=net-6.0)
- פרויקט NodaTime, ספריית תאריכים/זמנים חזקה עם תמיכה בקלנדרים שונים: [NodaTime](https://nodatime.org/)
