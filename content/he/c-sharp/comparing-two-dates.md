---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
כאשר אנחנו משווים שני תאריכים בתכנות, אנחנו בעצם מעריכים האם אחד מהם הוא לפני או אחרי השני. זה מאוד שימושי בגדלת אפליקציות אירוע, דיונים זמניים ותרגולים של מניפולציה של מסד נתונים.

‍‍## איך ל:
‍‍
```C#
using System;

class Program
{
    static void Main()
    {
        DateTime date1 = new DateTime(2021, 12, 1);
        DateTime date2 = new DateTime(2021, 12, 25);

        int result = DateTime.Compare(date1, date2);

        if (result < 0)
           Console.WriteLine("date1 is earlier than date2.");
        else if (result == 0)
           Console.WriteLine("date1 is the same as date2.");
        else
           Console.WriteLine("date1 is later than date2.");
    }
}
```
‍‍
‍‍פלט בדוגמא:
‍‍
```
date1 is earlier than date2.
```
‍‍## צלילה עמוקה
שפה של C# התפתחה במהלך השנים, ורעיונות חדשים התווספו כדי לכלול טיפול בזמן ותאריך. ישנם כמה דרכים אלטרנטיביות להשוות תאריכים ב-C#, אך שימוש ב-Datetime.Compare הוא הכי פשוט וברור.  
פרטי היישום של ההשוואה בין שני תאריכים מתבצעים באופן כמעט אוטומטי, תוך כדי שמירה על מידע הזמן והתאריך עצמו. כאשר משווים שני DateTime objects, הם משווים תחילה לפי השנה, אז החודש, ולבסוף היום.
‍‍
## ראה גם
[שפת התכנות C#](https://he.wikipedia.org/wiki/C_Sharp_(%D7%A9%D7%A4%D7%AA_%D7%AA%D7%9B%D7%A0%D7%95%D7%AA)) - ויקיפדיה  
[מחלקת DateTime ב-C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0) - התיעוד הרשמי של Microsoft  
[הכנה לקראת תאריך וזמן ב-C#](https://www.geekhideout.com/csharp-date-format.shtml)