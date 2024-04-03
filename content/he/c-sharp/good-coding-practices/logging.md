---
date: 2024-01-26 01:02:20.007517-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C#, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05DE\u05E8\u05D7\
  \u05D1 \u05D4\u05E9\u05DE\u05D5\u05EA \u05D4\u05E7\u05D9\u05D9\u05DD `System.Diagnostics`\
  \ \u05D0\u05D5 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 NLog \u05D0\u05D5 log4net. \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4 \u05DE\u05D4\u05D9\u05E8\u05D4 \u05E9\u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05EA \u05D1\u05DE\u05DE\u05E9\u05E7 `ILogger` \u05E9\u05D6\u05DE\
  \u05D9\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.353540-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-C#, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05DE\u05E8\u05D7\u05D1 \u05D4\u05E9\u05DE\u05D5\u05EA \u05D4\u05E7\u05D9\
  \u05D9\u05DD `System.Diagnostics` \u05D0\u05D5 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 NLog \u05D0\
  \u05D5 log4net."
title: "\u05DC\u05D5\u05D2\u05D9\u05DD"
weight: 17
---

## איך לעשות:
ב-C#, ניתן להשתמש במרחב השמות הקיים `System.Diagnostics` או בספריות צד שלישי כמו NLog או log4net. הנה דוגמה מהירה שמשתמשת בממשק `ILogger` שזמין ב-.NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("זהו הודעת מידע.");
        logger.LogWarning("זהו הודעת אזהרה.");
        logger.LogError("זהו הודעת שגיאה.");
    }
}
```

פלט לדוגמה:
```
info: Program[0]
      זהו הודעת מידע.
warn: Program[0]
      זהו הודעת אזהרה.
fail: Program[0]
      זהו הודעת שגיאה.
```

## עיון מעמיק
היסטוריית הרישום בפיתוח תוכנה היא כמעט כמו התכנות עצמה; התפתחה מפקודות הדפסה פשוטות למערכות מתוחכמות וניתנות לתצורה. במקור, רישום נעשה על ידי כתיבה לקבצים או לקונסול, אך זה התרחב לכלול עוד מבנים מורכבים כמו מערכות אגרגציה של לוגים ופלטפורמות ניתוח מבוזרות (כמו ערכת ELK או Jaeger).

חלופות לשיטת הרישום המובנית ב-.NET כוללות ספריות צד שלישי:
- **NLog**: גמישה וקלה להגדרה, עם הרבה תכונות למסלול, פורמט וסינון של לוגים.
- **log4net**: הושרא מספריה log4j של Java, ניתנת לתצורה גבוהה מתוך XML ותומכת במגוון מאגרי לוגים.

כאשר מדובר בפרטי היישום, בחירת מופשט הרישום שלך (כמו Microsoft.Extensions.Logging) וספק הרישום הבסיסי שלו יכולה להשפיע משמעותית על ביצועי האפליקציה והאמינות שלה. חשוב מאוד לקבוע בצורה נכונה את רמות הרישום ולוודא שכתיבת הלוגים אינה הופכת לנקודת חנק.

כמו כן, רישום מובנה - שבו אתה מתעד לא רק מחרוזות אלא זוגות מפתח-ערך או מופעים - מאפשר לקבל לוגים יותר מדויקים ושימושיים, שקל יותר לברר ולנתח.

## ראה גם
- [תיעוד Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [תיעוד NLog](https://nlog-project.org/documentation/)
- [תיעוד log4net](https://logging.apache.org/log4net/)
- [תיעוד Serilog](https://serilog.net/) (לדוגמה של רישום מובנה)
