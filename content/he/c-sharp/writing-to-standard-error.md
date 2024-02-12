---
title:                "כתיבה לשגיאה התקנית"
aliases:
- he/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:33:23.270072-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) ב-C# כוללת הכוונת הודעות שגיאה ואבחון בנפרד מהפלט הרגיל (stdout), כדי לעזור למשתמשים ולמפתחים להבדיל בין פלט התוכנית הרגיל להתראות שגיאה. מתכנתים עושים זאת כדי להפוך את הניפוי שגיאות והתיעוד ליעילים יותר, מה שמאפשר פעולה חלקה יותר ותחזוקה של יישומים.

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
