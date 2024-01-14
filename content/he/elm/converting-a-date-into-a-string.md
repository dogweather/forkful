---
title:    "Elm: המרת תאריך למחרוזת"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה: 
 כי חשוב להיות מסוגלים להציג תאריך כגון בתור טקסט באפליקציות או באתרים.

## כיצד:
 ###
 ```Elm
 import Time

dateToString : Time.Posix -> String
dateToString date =
    let
        month = Date.toMonth (Date.fromTime date)
        day = Date.day (Date.fromTime date)
        year = Date.year (Date.fromTime date)
    in
      (Month.toString month) ++ "-" ++ (toString day) ++ "-" ++ (toString year)

date = Time.millisToPosix 1632242400000
dateToString date --> "September-22-2021"
 ```

## התעמקות:

להמיר תאריך לטקסט באלם יכול להיעשות בעזרת הפונקציה Date.toMonth, Date.day ו- Date.year להשתמש באובייקט המכיל את התאריך וליצור ממנו טקסט ברוחב כמו התאריך הסטנדרטי. אם רוצים להיות מדויקים ניתן להשתמש בחיוג לכל אחד מהחודשים שיש כדי לוודא שהתאריך יוצג בדיוק כפי שציפיתם לו, לעומת זאת צריך לשים לב שצריך לתת רווחים וכוונות יתר עבור התאריך. בנוסף, כמובן שניתן להתאים את פורמט הטקסט בהתאם לצרכים שלכם ולהוסיף פרטים נוספים כמו יום בשבוע או שעות.

## ראה גם:
 - [Date.toMonth](https://package.elm-lang.org/packages/elm/time/latest/Time)
 - [Date.day](https://package.elm-lang.org/packages/elm/time/latest/Time)
 - [Date.year](https://package.elm-lang.org/packages/elm/time/latest/Time)
 - [תאריך באלם: התחברות, תאריך, שעות](https://www.bennadel.com/blog/3787-an-elm-datetime-exercise-meeting-planner.htm)