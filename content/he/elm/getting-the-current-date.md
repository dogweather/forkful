---
title:                "Elm: לקבלת תאריך נוכחי"
simple_title:         "לקבלת תאריך נוכחי"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מדוע

למה להשתמש בתאריך נוכחי? השימוש בתאריך נוח עבור מתכנתים המשתמשים ב-Elm יכול להיות שימושי למספר סיבות, כגון לתת תצוגה לתאריך הנוכחי למשתמש או לבצע פעולות חישוביות בתאריך הנוכחי.

# איך לעשות זאת

תחילה, צריך ליצור משתנה מסוג Date באמצעות פונקציית `now` המגיעה מספריה של `Time`:
```
Elm.Date.now
```
לאחר מכן, ניתן להשתמש בפונקציות נוספות כדי לקבל את המידע הרצוי מתאריך נוכחי, למשל:
```
import Date exposing (..)

getDate : Date -> Int
getDate date =
    date.day

getMonth : Date -> Month
getMonth date =
    date.month

-- פעולה להצגת תאריך נוכחי:
showCurrentDate =
    let
        currentDate =
            Date.now
    in
        String.fromInt (getDate currentDate) ++ "." ++ String.fromInt (getMonth currentDate) ++ "." ++ String.fromInt (currentDate.year)
```
והפלט של `showCurrentDate` יהיה:
```
"15.6.2021"
```

# חיפוש מחוקק

לקבלת עומק נוסף על איך להשתמש בתאריך נוכחי, ניתן לבקר בעמוד הרשמי של הספריה של Time בעזרת הקישורים המצורפים הלהלן.

# ראה גם

- [ספרייה של Time בתיעוד של Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [תאריך וזמן ב-Elm - סרטון הדרכה](https://www.youtube.com/watch?v=6WpaWmqBHrI)
- [הספריה Date בתיעוד של Elm](https://package.elm-lang.org/packages/elm/date/latest/Date)