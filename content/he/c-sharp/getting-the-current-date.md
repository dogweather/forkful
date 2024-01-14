---
title:    "C#: לקבלת תאריך נוכחי"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה?

קבלת תאריך נוכחי בתכנות נעשה בדרך כלל כדי לצרכי רישום ולבצע פעולות חשובות בתכנית. נצטרך לזוז ממצב רגיל שהנוכחי לא יתקיים או לשלוט על פעולות בתכנית בתאריך ספציפי.

## איך לעשות?

```C#
//שם משתנה לאחסון התאריך הנוכחי
DateTime currentDateTime = DateTime.Now;

//שימוש במתודה ToString על מנת להציג את התאריך בפורמט שלנו (מומלץ ללמוד את כל האפשרויות הקיימות)
Console.WriteLine(currentDateTime.ToString("dd/MM/yyyy"));

//הצגת התאריך בפורמט שלך עם שעות ודקות
Console.WriteLine(currentDateTime.ToString("dd/MM/yyyy HH:mm"));

//מנגד, נצטרך לשנות את התאריך לערך מספרי כמו 0.5 של יום
DateTime newDateTime = currentDateTime.AddDays(0.5);
Console.WriteLine(newDateTime.ToString("dd/MM/yyyy HH:mm"));

//ניתן גם לגשת לערכי התאריך כחלקים נפרדים לדוגמה שנים, חודשים וימים
Console.WriteLine(currentDateTime.Year);
Console.WriteLine(currentDateTime.Month);
Console.WriteLine(currentDateTime.Day);

//מתודות נוספות: AddHours, AddMinutes, AddSeconds וכו'.
```

**פלט:**

```
17/11/2021
17/11/2021 14:00
18/11/2021 02:00
2021
11
17
```

## כולל עומק

הפונקציה `DateTime.Now` מחזירה את התאריך הנוכחי הממוחשב מהמחשב המכליל את הזמן האוניברסלי. אופן נוסף לקבלת התאריך הנוכחי הוא להשתמש בפונקציה `DateTime.UtcNow` אשר מחזירה את התאריך הנוכחי הממוחשב מהמחשב כך שיולד באוניברסלי. יתר על כן, ניתן להשתמש בפונקציות נוספות כמו `IsLeapYear` לבדיקת שנת מעבר, `Now-AddMonths` לצירוף חודשים לתאריך הנוכחי ועוד.

## ראה גם

- [תיעוד רשמי בנוגע לפונקציות DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [ארגומנטים פופולרי