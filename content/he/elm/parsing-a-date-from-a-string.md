---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיבוד תאריך ממחרוזת הוא תהליך שבו מחשב מתייחס למחרוזת כנתונים מתאריך. תכנתים עושים את זה כדי לנתח ולהמיר את המחרוזות של התאריכים לאובייקטים מתאריך.

## איך לעשות:
ב- Elm, אנו משתמשים בספרייה 'elm/time' וב- 'elm/parse'. הינה דוגמה לקוד שיוצר לנו תאריך ממחרוזת:

```Elm
import Time
import Parser exposing ((|.), map, succeed)

iso8601 : String -> Maybe Time.Posix
iso8601 string =
    Parser.run dateParser string

dateParser : Parser Time.Posix
dateParser =
    Parser.succeed Time.millisToPosix
        |= (Time.year 4 |> map String.fromInt)
        |. spaces
        |= (Time.month Time.Jan |> int)
        |. spaces
        |= (Time.day 2 |> int )
```
מאבק השורות למעלה יוצר אובייקט dTime.Posix` ממחרוזת בפורמט "yyyy mm dd".

## שיעור מעמיק
בעבר, אנשים השתמשו בספריות של שלישים כמו Moment.js כדי לנתח מחרוזות תאריכים. מאז, העולם של Javascript התפתח, ומשתמשים מועדפים להשתמש בפונקציות תאריך בנויות בעידן החדיש. בעוד שלמידה לנתח את התאריך ממחרוזת באמצעות Elm יכול לקחת קצת זמן, היא הרבה יותר מאובטחת מהפתרונות הנפוצים של αs strftime או String.

## ראה גם
דוקומנטציה ל- 'elm/time': https://package.elm-lang.org/packages/elm/time/latest/
ספרייה 'elm/parser': https://package.elm-lang.org/packages/elm/parser/latest/