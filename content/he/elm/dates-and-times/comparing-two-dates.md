---
date: 2024-01-20 17:33:11.329192-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D1-Elm \u05D4\u05D9\u05D9\u05EA\u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05D9\u05D5\u05EA\u05E8 \u05DE\u05E1\u05D5\u05D1\u05DA, \u05DE\u05DB\u05D9\u05D5\
  \u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05D3\u05E8\u05E9\u05D4 \u05D4\u05DE\u05E8\
  \u05D5\u05EA \u05D9\u05D3\u05E0\u05D9\u05D5\u05EA \u05E8\u05D1\u05D5\u05EA. \u05D0\
  \u05DC\u05D8\u05E8\u05E0\u05D8\u05D9\u05D1\u05D5\u05EA \u05DB\u05D5\u05DC\u05DC\u05D5\
  \u05EA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\
  \ \u05E9\u05DC\u05D9\u05E9\u05D9\u05D5\u05EA \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\
  \u05EA \u05E7\u05D5\u05D3 \u05E2\u05D6\u05E8\u2026"
lastmod: '2024-04-05T21:53:40.436271-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-Elm \u05D4\u05D9\u05D9\u05EA\u05D4 \u05EA\u05D4\
  \u05DC\u05D9\u05DA \u05D9\u05D5\u05EA\u05E8 \u05DE\u05E1\u05D5\u05D1\u05DA, \u05DE\
  \u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05D9\u05D0 \u05D3\u05E8\u05E9\u05D4\
  \ \u05D4\u05DE\u05E8\u05D5\u05EA \u05D9\u05D3\u05E0\u05D9\u05D5\u05EA \u05E8\u05D1\
  \u05D5\u05EA."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
```Elm
import Time
import Time.Extra

compareDates : Time.Posix -> Time.Posix -> Order
compareDates date1 date2 =
    Time.Extra.compare date1 date2

-- דוגמה לקריאה ולפלט
let
    date1 = Time.millisToPosix 1500000000000
    date2 = Time.millisToPosix 1600000000000
in
compareDates date1 date2  -- הפלט יהיה: LT (Less Than)
```

## עיון מעמיק:
בעבר, השוואת תאריכים ב-Elm הייתה תהליך יותר מסובך, מכיוון שהיא דרשה המרות ידניות רבות. אלטרנטיבות כוללות שימוש בספריות שלישיות או כתיבת קוד עזר משלך. פרטי המימוש כוללים פונקציונליות שמשווה סמני זמן POSIX, שהם נקודות בזמן מאז תחילת תקן UTC ב-1 בינואר 1970.

## ראה גם:
- המסמכים הרשמיים של Elm לספריית [Time](https://package.elm-lang.org/packages/elm/time/latest/)
- ספריית [Time.Extra](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/) לפונקציות עזר נוספות
- הדיון ב[Elm Discourse](https://discourse.elm-lang.org/) על השוואת תאריכים
