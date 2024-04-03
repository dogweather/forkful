---
date: 2024-01-20 17:45:50.860875-07:00
description: "How to: \u05D1-Elm, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05DB\u05DE\u05D5 `String.slice` \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D7\u05EA\
  \ \u05D7\u05DC\u05E7\u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05D3\
  \u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.183557-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Elm, \u05D0\u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DB\
  \u05DE\u05D5 `String.slice` \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D7\u05EA \u05D7\u05DC\
  \u05E7\u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
weight: 6
---

## How to:
ב-Elm, אתה יכול להשתמש בפונקציות כמו `String.slice` כדי לקחת חלקים ממחרוזת. דוגמה:

```Elm
import String

main =
    let
        original = "שלום, עולם!"
        substring = String.slice 6 11 original
    in
    text substring
```

הפלט יהיה:

```
עולם
```

## Deep Dive
בעבר, שפות תכנות שונות התמודדו אית חילוץ תת-מחרוזות בדרכים רבות. ב-Elm, הפונקציה `String.slice` היא הפופולרית ביותר לכך. `String.slice` מקבלת שני מפתחות, התחלה וסוף, ותחזיר את החלק ביניהם. חשוב להבין שהאינדקס של הסיום לא כלול. אם אינדקס ההתחלה גדול מאינדקס הסיום, היא תחזיר מחרוזת ריקה.

חלופות כוללות שימוש ב- `String.left` ו- `String.right` לחילוץ מחרוזות מהתחלה או סוף בהתאמה, אבל פחות גמישות.

## See Also
קרא עוד במסמכי Elm:
- [String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [String](https://package.elm-lang.org/packages/elm/core/latest/String)

לעומק יותר על מחרוזות ב-Elm, בדוק:
- [An Introduction to Elm Strings](https://elmprogramming.com/string.html)
