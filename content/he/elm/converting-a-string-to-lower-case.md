---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Elm: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה?

למה להתעסק בהמרת מחרוזת לאותיות קטנות? כי לעיתים קרובות ישנן מצבים שבהם אנחנו צריכים להתמודד עם כתיבת מחרוזת באותיות גדולות או משתנות הכוללות כתיבה באותיות גדולות. להמיר את המחרוזת לאותיות קטנות יכול לעזור לנו להפסיק את הפעולה המתבצעת באנווכדות ולהבטיח שהתוצאה המוחזרת תהיה תואמת לכתיב הרגיל שלנו.

## איך לעשות?

השתמשו בפונקציה `String.toLower` כדי להמיר מחרוזת לאותיות קטנות בספרייה של ה-Elm. הנה דוגמה של קוד המשתמש בפונקציה זו והתוצאה המוחזרת:

``` Elm
import String

String.toLower "ELM" -- תוצאה: "elm"
```

כמו כן, תוכלו להשתמש בפונקציה `String.map` כדי להמיר יחידות בתוך המחרוזת לאותיות קטנות. הנה דוגמה נוספת:

``` Elm
import String

String.map (\char ->
    if Char.isUpper char then
        Char.toLower char
    else
        char
) "Elm" -- תוצאה: "elm"
```

## חפירה עמוקה

המרת מחרוזת לאותיות קטנות היא פעולה פשוטה ונפוצה בתכנות. באמצעות השתמשות בפונקציות `String.toLower` ו-`String.map` ניתן לנהל את הטיפול ביחידות ומחרוזות בצורה נקייה ויעילה.

## ראו גם

- תיעוד רשמי לפונקציות `String.toLower` ו-`String.map` באתר המפתחים של ה-Elm: https://package.elm-lang.org/packages/elm/core/1.0.5/String
- מאמר נוסף על פונקציות תחילת מחרוזת ותוצאתן ב־Elm: https://-programmingbasics.org/en/line-spacing/elm-of-string-functions