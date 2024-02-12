---
title:                "המרת תאריך למחרוזת"
aliases:
- /he/elm/converting-a-date-into-a-string/
date:                  2024-01-20T17:36:44.519393-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת אומרת שאתה לוקח אובייקט שמייצג תאריך ושם זמן ושומר אותו כטקסט. תוכניתנים עושים את זה כדי להציג תאריכים למשתמשים או לשמור פורמטים עקביים לנתונים.

## איך לעשות:
```Elm
import Time exposing (Posix)
import Time.Zone exposing (Zone)
import Task

-- יצירת פונקציה שממירה תאריך למחרוזת
toDateString : Zone -> Posix -> String
toDateString zone posix =
    Time.toUtc zone posix
        |> Time.posixToMillis
        |> String.fromInt
        |> (\millis -> millis ++ " UTC")

-- דוגמה לשימוש בפונקציה
example : Task.Task Time.Error String
example =
    Task.map (toDateString Time.utc) (Time.now)

-- ריצת הקוד תחזיר מחרוזת שמייצגת את התאריך והזמן כרגע בפורמט UTC
```
פלט לדוגמה:
```
"1617850794000 UTC"
```

## נפך מעמיק:
בעבר, מרבית השפות סיפקו ספריות תאריך ברירת מחדל, ול-Elm יש 'Time' לניהול תאריכים. ישנן גם ספריות חיצוניות כמו 'ryannhg/date-format' הנותנות תמיכה נוספת בפורמטינג. ביצוע המרה מחייב להתחשב באזורי זמן ובפורמט הרצוי של המחרוזת. Elm מאפשר לעבוד עם פוסיק (Posix) זמן, המתאר זמן במילישניות מאז ינואר 1970, המכונה גם 'Unix Epoch'.

## ראה גם:
- [תיעוד החלק של הזמנים ב-Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [ryannhg/date-format](https://package.elm-lang.org/packages/ryannhg/date-format/latest)
- [טיפול באזורי זמן ב-Elm](https://package.elm-lang.org/packages/elm/time/latest/Time-Zone)
