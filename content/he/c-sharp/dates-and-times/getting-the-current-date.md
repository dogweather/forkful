---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:39.193786-07:00
description: "\u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-C# \u05DB\u05D5\u05DC\u05DC \u05D0\
  \u05D9\u05E1\u05D5\u05E3 \u05E4\u05E8\u05D8\u05D9 \u05D4\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\
  \u05DD \u05DE\u05D4\u05DE\u05E2\u05E8\u05DB\u05EA. \u05DC\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05D9\u05E9 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05E6\u05D5\u05E8\u05DA \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\
  \u05D9\u05D3\u05E2 \u05D6\u05D4 \u05D1\u05E9\u05D1\u05D9\u05DC \u05DC\u05EA\u05E2\
  \u05D3, \u05DC\u05D1\u05E6\u05E2 \u05EA\u05D9\u05D5\u05D2 \u05D6\u05DE\u05DF \u05DC\
  \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA,\u2026"
lastmod: '2024-03-11T00:14:12.812615-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-C# \u05DB\u05D5\u05DC\u05DC \u05D0\
  \u05D9\u05E1\u05D5\u05E3 \u05E4\u05E8\u05D8\u05D9 \u05D4\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9\u05D9\
  \u05DD \u05DE\u05D4\u05DE\u05E2\u05E8\u05DB\u05EA. \u05DC\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05D9\u05E9 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05E6\u05D5\u05E8\u05DA \u05DC\u05D2\u05E9\u05EA \u05DC\u05DE\
  \u05D9\u05D3\u05E2 \u05D6\u05D4 \u05D1\u05E9\u05D1\u05D9\u05DC \u05DC\u05EA\u05E2\
  \u05D3, \u05DC\u05D1\u05E6\u05E2 \u05EA\u05D9\u05D5\u05D2 \u05D6\u05DE\u05DF \u05DC\
  \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA,\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי ב-C# כולל איסוף פרטי התאריך והשעה הנוכחיים מהמערכת. לתכנתים יש לעיתים קרובות צורך לגשת למידע זה בשביל לתעד, לבצע תיוג זמן לפעולות, או לתזמן משימות בתוך אפליקציות, ובכך להבטיח שהפעולות מתוזמנות במדויק והנתונים מתויגים בחותמות זמן מדויקות.

## איך לעשות:
C# מספקת דרך פשוטה לקבל את התאריך הנוכחי באמצעות המחלקה `DateTime` שהיא חלק ממרחב השמות System של .NET Framework. הדוגמא למטה מדגימה איך לקבל את התאריך הנוכחי, ואופציונלית, את השעה.

```csharp
using System;

class Program
{
    static void Main()
    {
        // מקבל את התאריך הנוכחי בלבד
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // פלט: MM/dd/yyyy
        
        // מקבל את התאריך והשעה הנוכחית
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // פלט: MM/dd/yyyy HH:mm:ss

        // מקבל את התאריך והשעה הנוכחיים ב-UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // פלט: MM/dd/yyyy HH:mm:ss
    }
}
```

בנוגע לספריות של צד שלישי, NodaTime מציעה חלופה נוקשה למניפולציה של תאריך ושעה, כולל איסוף התאריך הנוכחי בלוחות שנה ואזורי זמן שונים.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // בשימוש של NodaTime כדי לקבל את התאריך הנוכחי בלוח השנה ה-ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // פלט: yyyy-MM-dd

        // לתאריכים שתלויים באזור זמן
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // פלט: yyyy-MM-dd
    }
}
```

זה מציג את השימוש הבסיסי עם המחלקה המובנית `DateTime` והיכולות המתקדמות שמספק NodaTime, במיוחד מתאים לאפליקציות שדורשות טיפול באזורי זמן או מערכות לוח שנה שונים.
