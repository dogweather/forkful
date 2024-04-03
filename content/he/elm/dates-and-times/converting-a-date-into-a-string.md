---
date: 2024-01-20 17:36:44.519393-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D5\u05DE\u05E8\u05EA \u05E9\u05D0\u05EA\
  \u05D4 \u05DC\u05D5\u05E7\u05D7 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05E9\
  \u05DE\u05D9\u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05DD\
  \ \u05D6\u05DE\u05DF \u05D5\u05E9\u05D5\u05DE\u05E8 \u05D0\u05D5\u05EA\u05D5 \u05DB\
  \u05D8\u05E7\u05E1\u05D8. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D4\u05E6\u05D9\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E9\u05DE\u05D5\u05E8\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD \u05E2\u05E7\u05D1\u05D9\u05D9\u05DD\
  \u2026"
lastmod: '2024-03-13T22:44:39.221502-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D0\u05D5\u05DE\u05E8\u05EA \u05E9\u05D0\u05EA\u05D4\
  \ \u05DC\u05D5\u05E7\u05D7 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05E9\u05DE\
  \u05D9\u05D9\u05E6\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05DD \u05D6\
  \u05DE\u05DF \u05D5\u05E9\u05D5\u05DE\u05E8 \u05D0\u05D5\u05EA\u05D5 \u05DB\u05D8\
  \u05E7\u05E1\u05D8."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

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
