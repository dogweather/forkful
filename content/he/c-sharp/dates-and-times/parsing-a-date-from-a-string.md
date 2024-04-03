---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:16.097114-07:00
description: "\u05DC\u05E2\u05D1\u05D3 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-C# \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\
  \u05D4 \u05E9\u05DC \u05D9\u05D9\u05E6\u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\
  \u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD \u05DC\u05D0\u05D5\u05D1\
  \u05D9\u05D9\u05E7\u05D8 `DateTime`. \u05D6\u05D4 \u05D7\u05D9\u05D5\u05E0\u05D9\
  \ \u05DC\u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD \u05E9\u05E6\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05DC\u05E9\u05E0\u05D5\u05EA, \u05DC\u05D0\u05D7\u05E1\u05DF \u05D0\
  \u05D5 \u05DC\u05D4\u05E6\u05D9\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \u2026"
lastmod: '2024-03-13T22:44:39.358385-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05E2\u05D1\u05D3 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1-C# \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05D4\
  \ \u05E9\u05DC \u05D9\u05D9\u05E6\u05D5\u05D2\u05D9\u05DD \u05D8\u05E7\u05E1\u05D8\
  \u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\
  \u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD \u05DC\u05D0\u05D5\u05D1\u05D9\
  \u05D9\u05E7\u05D8 `DateTime`."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## מה ולמה?
לעבד תאריך ממחרוזת ב-C# כולל המרה של ייצוגים טקסטואליים של תאריכים וזמנים לאובייקט `DateTime`. זה חיוני ליישומים שצריכים לשנות, לאחסן או להציג תאריכים וזמנים בפורמטים שונים, כמו אפליקציות לוח שנה, מעבדי לוגים, או כל מערכת שמטפלת בקלט תאריכים ממשתמשים או ממקורות חיצוניים.

## איך לעשות:

**ניתוח בסיסי:**

המתודות `DateTime.Parse` ו`DateTime.TryParse` הן האפשרויות העיקריות להמיר מחרוזת ל`DateTime`. הנה דוגמה מהירה:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"הניתוח הצליח: {parsedDate}");
}
else
{
    Console.WriteLine("נכשל בניתוח.");
}
// Output: הניתוח הצליח: 4/12/2023 12:00:00 AM
```

**ציון תרבות:**

לעיתים, יש צורך לנתח מחרוזת תאריך שנמצאת בפורמט תרבותי מסוים. ניתן להשיג זאת באמצעות המחלקה `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Output: 4/12/2023 12:00:00 AM
```

**ניתוח מדויק עם פורמט מסוים:**

לסצנריות שבהן התאריכים מגיעים בפורמט מסוים שעלול שלא להיות סטנדרטי, `DateTime.ParseExact` יכול לעזור:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Output: 4/12/2023 12:00:00 AM
```

**שימוש ב-NodaTime:**

לניתוח תאריכים וזמנים עוד יותר חזק, שקול להשתמש בספרייה הפופולרית מבית שלישי NodaTime. היא מספקת מגוון רחב יותר של יכולות טיפול בתאריכים/זמנים:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("נכשל בניתוח.");
}
```

NodaTime מציעה תמיכה נרחבת באזורי זמן, מושגי תקופה ומשך זמן, ומערכות לוח שנה שונות, הופכת אותה לבחירה עוצמתית למניפולציות מורכבות של תאריכים וזמנים ביישומי .NET.
