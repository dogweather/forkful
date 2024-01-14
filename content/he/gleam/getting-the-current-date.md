---
title:                "Gleam: מקבלים את התאריך הנוכחי"
simple_title:         "מקבלים את התאריך הנוכחי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה

רק 1-2 משפטים המסבירים *למה* מישהו ירצה להתעסק בקבלת התאריך הנוכחי.

## איך לעשות זאת

בדוגמאות קוד ופלט מתוך קבוצות הקוד "```Gleam ... ```".

```Gleam
import gleam/time

current_date = time.now()
time_string = time.format(current_date, "%Y-%m-%d")
time_string |> IO.println

```

פלט:

```
2021-07-06
```

## לחקור עומק

לעומק על קבלת התאריך הנוכחי:

להתעשק יותר מקבלת תאריך יש לנו את הספריה **`gleam/time`** שמספקת לנו פונקציות משתנות כדי לעבוד עם תאריך. נוכל להשתמש בפונקציה **`format`** כדי להמיר את התאריך לתבנית מסוימת. בנוסף, אם נרצה להציג יותר מפרטים על התאריך, נוכל להשתמש בפונקציה **`decode`** כדי לקבל את התאריך בפורמט המקורי. תוכלו למצוא עוד מידע על פונקציות אלה ועל יישומן במסמך הרישמי של גלים העומד תחת כותרת "גלים פונקצית הזמן".

## ראו גם

- [התיעוד הרישמי של גלים](https://gleam.run/doc) 
- [מדריך לפתרונות שגרות קומונים עם גלים פונקצית הזמן](https://blog.gleam.run/building-common-solutions-with-gleam-time-function)
- [תיעוד על גלים פונקצית הזמן (באנגלית)](https://hexdocs.pm/gleam/gleam/time.html)