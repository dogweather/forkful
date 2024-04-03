---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:23.270072-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C#, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E9\u05D2\u05D9\u05D0\
  \u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05D1\u05D0\u05DE\u05E6\
  \u05E2\u05D5\u05EA \u05D6\u05E8\u05DD \u05D4`Console.Error`. \u05D6\u05E8\u05DD\
  \ \u05D6\u05D4 \u05DE\u05E9\u05DE\u05E9 \u05D1\u05DE\u05D9\u05D5\u05D7\u05D3 \u05E2\
  \u05D1\u05D5\u05E8 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\
  \u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05DF. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\
  \u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\u05EA."
lastmod: '2024-03-13T22:44:39.370136-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C#, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\
  \u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D6\u05E8\u05DD \u05D4`Console.Error`."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות:
ב-C#, ניתן לכתוב לשגיאה סטנדרטית באמצעות זרם ה`Console.Error`. זרם זה משמש במיוחד עבור הודעות שגיאה ואבחון. הנה דוגמה בסיסית:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

דוגמת פלט (ל-stderr):
```
Error: Failed to process the request.
```

לסצנריות שבהן אתם עשויים להשתמש בספרייה צד שלישי שמציעה יכולות תיעוד מתקדמות, כמו `Serilog` או `NLog`, תוכלו להגדיר את הספריות הללו לכתוב יומני שגיאות ל-stderr. למרות שהדוגמאות הללו מתמקדות בהפניית קונסול פשוטה, זכרו שביישומים ייצוריים, מסגרות תיעוד מציעות אפשרויות טיפול בשגיאות ופלט יותר חזקות. הנה דוגמה פשוטה עם `Serilog`:

ראשית, התקינו את החבילה של Serilog ואת הברז שלה לקונסול:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

לאחר מכן, הגדירו את Serilog לכתוב ל-stderr:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

דוגמת פלט (ל-stderr עבור הודעת השגיאה):
```
[15:04:20 ERR] This is an error message.
```

הערה: ההגדרה `standardErrorFromLevel` בברז הקונסול של Serilog מפנה את כל אירועי התיעוד ברמת הקובעת המצוינת (שגיאה, במקרה זה) או גבוהה יותר לזרם השגיאה הסטנדרטית, בעוד שהודעות ברמות נמוכות יותר כמו מידע נכתבות לזרם הפלט הסטנדרטי.
