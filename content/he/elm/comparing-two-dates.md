---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה & למה?
השוואה של שני תאריכים היא פעולה המאפשרת לנו לבדוק האם תאריך אחד מוקדם, יותר מאוחר או שווה לתאריך השני. מתכנתים עשויים להשתמש בכך כדי לבצע השוואות זמן באפליקציות כמו לוחות שנה או עבודה עם נתונים ממוחשבים.

## כיצד:
באמצעות פונקציה `compare` במודול `Date`, אפשר להשוות בין שני תאריכים. 

```Elm
import Date exposing (fromString, compare)
import Maybe exposing (withDefault)

date1 = withDefault (Date.fromString "2000-01-01") (Date.fromString "2020-12-25")
date2 = withDefault (Date.fromString "2000-01-01") (Date.fromString "2025-05-01")

main = 
    let
        comparison = compare date1 date2
    in
    case comparison of
        LT -> 
            Debug.log "date1 is earlier than date2"
        GT -> 
            Debug.log "date1 is later than date2"
        EQ -> 
            Debug.log "date1 is the same as date2"
```

## עומק מרובה:
הפונקציה `compare` במודול `Date` מאפשרת למתכנתים להשוות תאריך לתאריך אחר באופן יעיל ומאובטח, מתוך שימת דגש על ביצועים ודיוק. חלופות אחרות, כמו המרת התאריכים לצורת טקסט והשוואה של המחרוזות, עשויות להיות בעייתיות כאשר אין דיוק מוחלט.

## ראו גם:
1. [Elm Date](https://package.elm-lang.org/packages/elm/time/latest/Date): החבילה הרשמית של Elm לעבודה עם תאריכים.
2. [Github - Elm Examples](https://github.com/evancz/elm-architecture-tutorial): דוגמאות לארכיטקטורת Elm, כולל עיבוד תאריכים.