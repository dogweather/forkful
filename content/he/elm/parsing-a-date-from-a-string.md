---
title:                "מפרש תאריך ממחריז"
html_title:           "Elm: מפרש תאריך ממחריז"
simple_title:         "מפרש תאריך ממחריז"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
לפרק מידע מתאריך בתוך מחרוזת הוא פעולה שתכונה את תאריך מוגדר מחדר תווים, כך שאפשר להשתמש בו באופן מבנה ולעבוד עם הנתונים הזמינים. תהליך זה נחשב לא רק ראוי לתכנות יישומים, אלא גם חשוב לזיהוי וניהול תאריכים בדרך טבעית.

## כיצד לעשות זאת:
```Elm
import Date

Date.fromString "2021-10-25" -- כתובת דוגמה
-- Just (Date.fromTime 0)

Date.fromString "asdf" -- כתובת דוגמה שגויה
-- Nothing
```

## בינתיים חיפוש
הקונקטסט ההיסטורי של פרק מידע מתאריך בתוך מחרוזת מתייד את שימוש העתיד שלו: תוכניות תאריכים ותאריכי יצירה. פתרונות תומכים בפרק מידע הם כלי מתקדם שמציעים המון יכולות למפתחים.

פרוייקטים עבור פרק מידע הם מפעילים להשתמש בפונקציות המבוססות על הספקים המקוריים עבור מאגר התאריכים. אפשר ליישם או לשפר את אותה רעיונות מסביר טוב פקיני ללא התלות בתאוות פרק מידע. הסיכום הקשוח הוא שמומלץ מאוד להשתמש בפרק מידע כחלק מכל יישום פרק תאריכים.

## ראו גם:
- [פריטי תאריך](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#Date)
- [פונקציות תאריך](https://package.elm-lang.org/packages/elm/time/latest/Time-Date#functions)