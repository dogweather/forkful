---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:35:50.082655-07:00
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הניתוח של תאריכים ממחרוזות ב-C# הוא התהליך שבו אנו ממירים טקסט לאובייקט `DateTime`. מתכנתים עושים זאת כדי לעבוד עם תאריכים בצורה מסודרת ולמנוע בעיות עם פורמטים שונים של תאריכים.

## איך לעשות:
הנה דוגמא של קוד שניתח תאריך ממחרוזת:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "24/01/2023"; // דוגמה למחרוזת תאריך
        DateTime parsedDate;

        if (DateTime.TryParseExact(dateString, "dd/MM/yyyy", 
            CultureInfo.InvariantCulture, DateTimeStyles.None, out parsedDate))
        {
            Console.WriteLine($"התאריך שניתח: {parsedDate}");
        }
        else
        {
            Console.WriteLine("לא הצלחנו לנתח את התאריך.");
        }
    }
}
```

תוצאת הקוד הזה תהיה:
```
התאריך שניתח: 24/01/2023 00:00:00
```

## עמוק יותר:
בזמן המוקדם של .NET, מתכנתים השתמשו בפונקציה `DateTime.Parse()` אבל היא לא תמיד הייתה בטוחה מקריסה או שגיאות בפורמט. `DateTime.TryParse()` ו`DateTime.TryParseExact()` נוצרו כדי לתת יותר שליטה ובטחון. הפונקציה `TryParseExact` מאפשרת לך להגדיר במפורש את הפורמט של התאריך שאתה מצפה, בעוד ש`TryParse` נותנת יותר גמישות לפורמטים אפשריים.

השתמש ב`CultureInfo` כדי להבטיח שהפורמט של התאריך מתאים לאזור מסוים. כמו כן, אפשר להשתמש ב`DateTimeStyles` כדי להוסיף הגדרות ספציפיות, כמו לדלג על תווים לא חשובים במחרוזת.

לחלופין, ניתן להשתמש בספריות פרטיות כמו NodaTime שנותנות יותר גמישות ודיוק לעבודה עם תאריכים וזמנים, אך לא תמיד הכרחיות.

## עיון נוסף:
- [Documentation of DateTime.TryParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact?view=net-6.0)
- [NodaTime Documentation](https://nodatime.org/)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
