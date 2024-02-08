---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:11.329192-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים היא פשוט המצאת ההבדלים או הסידור שלהם בסדר כרונולוגי. תכנתים עושים זאת כדי לנהל לוחות זמנים, לבדוק את התוקף של קופונים, ולאכוף הגבלות בזמן.

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
