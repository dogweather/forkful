---
title:                "קבלת תאריך נוכחי"
html_title:           "C#: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מדוע
ישנם מצבים רבים בתכנות שבהם נדרש להשתמש בתאריך הנוכחי, לדוגמה ביצירת יומן יומי או בדיקת תקינות של ערכים של תאריכים. תכנות בקשה זו קל יותר כאשר יש לנו דרך פשוטה לקבל תאריך נוכחי באמצעות שפת תכנות כמו C#.

## איך לעשות זאת
מטרתנו היא לקבל את התאריך הנוכחי בפורמט של יום/חודש/שנה. נוכל לעשות זאת בקלות באמצעות הפקודה DateTime.Now בשפת C#. הנה דוגמא:

```C#
DateTime currentDateTime = DateTime.Now;
Console.WriteLine("תאריך נוכחי: {0}/{1}/{2}", currentDateTime.Day, currentDateTime.Month, currentDateTime.Year);
```

פלט:

```C#
תאריך נוכחי: 11/06/2021
```

ניתן גם לקבל את השעה הנוכחית בנוסף לתאריך כך:

```C#
DateTime currentTime = DateTime.Now;
Console.WriteLine("שעה נוכחית: {0}:{1}:{2}", currentTime.Hour, currentTime.Minute, currentTime.Second);
```

פלט:

```C#
שעה נוכחית: 12:23:45
```

## הכנסה עמוקה
כדי להבין טוב יותר את השימוש בתאריך הנוכחי בשפת C#, נוכל לחקור קצת יותר על הפונקציה DateTime.Now. פונקציה זו מציגה תאריך ושעה נוכחיים במחשב המשתמש. בנוסף, ניתן לשנות את הפורמט של התאריך והשעה באמצעות פונקציות נוספות כמו ToShortDateString() ו- ToShortTimeString(). לדוגמה:

```C#
DateTime currentDateTime = DateTime.Now;
Console.WriteLine("תאריך נוכחי בפורמט קצר: {0}", currentDateTime.ToShortDateString());
```

פלט:

```C#
תאריך נוכחי בפורמט קצר: 11/06/2021
```

ניתן גם לייצא את התאריך והשעה לפורמט אחר של אותיות ומספרים, באמצעות פונקציה כמו ToString() ועל ידי הכנסת מחרוזת עם פורמט מבוקש כפרמטר. לדוגמה:

```C#
DateTime currentDateTime = DateTime.Now;
string customFormat = current