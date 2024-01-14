---
title:    "C#: חישוב תאריך בעתיד או בעבר"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

מחשבון התאריכים הוא כלי חיוני לכל מתכנת בכל שפת תכנות. הוא מאפשר לנו לחשב תאריכים בעתיד או בעבר עם קלות ומהירות. הפונקציונליות הזו ניתנת לשימוש במגוון מקרים, כגון חישוב יום הולדת, תאריך תפוגה של אירוע, או כל מטרה אחרת חלופית שדורשת חישוב תאריך מדויק.

## איך לעשות זאת

לדוגמה, כדי לחשב תאריך בעתיד החל מהיום, ניתן להשתמש בפונקציה `AddDays()` כך:

```C#
DateTime currentDate = DateTime.Now; // יום של היום
DateTime futureDate = currentDate.AddDays(365); // יום של תאריך אחר
Console.WriteLine(futureDate); // פלט: יום של היום הוא בעוד 365 ימים
```

כדי לחשב תאריך בעבר, ניתן להשתמש בפונקציה `AddDays()` עם מספר שלילי כמו כן:

```C#
DateTime currentDate = DateTime.Now; // יום של היום
DateTime pastDate = currentDate.AddDays(-30); // יום של תאריך קודם
Console.WriteLine(pastDate); // פלט: יום של היום היה לפני 30 ימים
```

## חפירה עמוקה

חישוב תאריך בעתיד או בעבר הוא תהליך מורכב המשתמש בכמה נקודות חשובות. כאשר מחשבים תאריכים בעתיד, חשוב לשים לב לצרכי העסקיים והמטרות של האלמנט הזה. בנוסף, יש לקחת בחשבון גם את ההבנה הארגונומית בשימוש בתאריכים בתוך מערכות מחשב וכיצד לטפל בכל השמתקעות אפשריות.

## ראה גם

- [תיעוד רשמי של פונקציונליות תאריך ושעה בשפת C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [מדריך תאריך ושעה ב-C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [התאריך והשעה בשפת C#: טיפים וחקירה עמקית](https://