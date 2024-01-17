---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה זה ולמה זה חשוב:

לקבלת התאריך הנוכחי היא פעולה חשובה בתחום התכנות ובמיוחד בשפת C#. התאריך הנוכחי משמש כמידע חיוני ביישומים רבים כמו מערכות תאריך ושעה, מועדי תאריך ועוד. בנוסף, תוכנתנים יכולים להשתמש בתאריך הנוכחי כדי לרכז מידע וליצור חישובים משבועיים או חודשיים.

## איך לעשות:

```C#
// קבלת התאריך הנוכחי בשפת C#
DateTime currentDate = DateTime.Now;

// קבלת תאריך נוכחי בפורמט מותאם אישית
string customDate = DateTime.Now.ToString("dd/MM/yyyy");

// קבלת תאריך נוכחי בפורמט מספרי
int currentDay = DateTime.Now.Day;
int currentMonth = DateTime.Now.Month;
int currentYear = DateTime.Now.Year;

// פלט:
// מנת ליצור פלט יותר אסתטי, נשתמש בפונקצית WriteLine
Console.WriteLine("התאריך הנוכחי הוא: " + currentDate);
Console.WriteLine("התאריך הנוכחי בפורמט מותאם אישית הוא: " + customDate);
Console.WriteLine("היום הנוכחי בחודש הוא: " + currentDay);
Console.WriteLine("החודש הנוכחי הוא: " + currentMonth);
Console.WriteLine("השנה הנוכחית היא: " + currentYear);

// פלט:
// התאריך הנוכחי הוא: 04/12/2021 19:30:00
// התאריך הנוכחי בפורמט מותאם אישית הוא: 04/12/2021
// היום הנוכחי בחודש הוא: 04
// החודש הנוכחי הוא: 12
// השנה הנוכחית היא: 2021
```

## חפירה עמוקה:

לקבלת התאריך הנוכחי יש שפע שימושים רבים ופונקציות נוספות הקשורות אליו. עבור רוצה לפתח אפליקציית יישום תאריך ושעה, זה חיוני להשתמש בהגדרה נכונה של התאריך הנוכחי על מנת להפעל את המערכת היחידה.

בשפת C#, ישנן מספר אפשרויות לקבלת התאריך הנוכחי, כולל להשתמש במיניינג "DateTimeOffset" כדי לקבל את התאריך והשעה הנוכחיים לפי התאריך של המחשב שולח פנימה. ישנם גם שיטות לשינוי וניסוח התאריך הנוכחי ולעבוד עם מועדים נוספים כמו יום העבודה הנוכחי או מכילת הזמן הנוכחי.

## ראה גם:

למידע נוסף על דרכי השימוש בתאריך הנוכחי ושימושיו בשפת C#, ניתן לבדוק את התיעוד הרשמי של Microsoft על DateTime כאן: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0.

חוץ מזה, ניתן למצוא מידע נוסף על שימושים נוספים כמו DateTime.Parse לשינוי מחרוזת לתאריך וDateTimeFormatInfo לניהול פורמט תאריך מותאות בפנייה לתיעוד נוסף מהמקור המצורת של ספריית .NET ממיקרוסופט.