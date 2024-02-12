---
title:                "חילוץ תת-מחרוזות"
date:                  2024-01-20T17:45:50.860875-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה חלקי תת-מחרוזות ולמה צריך אותם? זה הפעולה של לקיחת חתיכת טקסט מתוך מחרוזת קיימת. תכנותים עושים את זה בשביל לעבד ולנתח מידע סציפי ממחרוזות.

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
