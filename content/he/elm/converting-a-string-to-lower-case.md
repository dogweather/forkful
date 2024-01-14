---
title:                "Elm: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה
רבים מתכנתי Elm קוראים לפונקציה שממירה מחרוזת לאותיות קטנות "את חשובה מאוד" - פשוט וסופר שימושי!

## כיצד לעשות זאת
בעזרת הפונקציה `String.toLower` נוכל להמיר את המחרוזת שנמצאת בתוך הקלט שלנו לאותיות קטנות. לדוגמה:

```Elm
String.toLower "HELLO" -- "hello"
```

בנוסף, ניתן להשתמש גם בחיבור של פונקציות, לדוגמה:

```Elm
String.toLower (String.concat [ "h", "e", "l", "l", "o" ]) -- "hello"
```

## מעמקים
המרת מחרוזת לאותיות קטנות נעשית על ידי החלפת כל אות גדולה במחרוזת לאות קטנה בעזרת פונקציית `Char.toLower`. בנוסף, אם המחרוזת מכילה תווים לא אלפאביתיים, הם יישארו ללא שינויים.

## ראו גם
- [מדריך התחשבות בהעצמת נסיעת כביש](https://ohanhi.github.io/writing/2017-03-15-Elm-type-uplifting/)
- [המרת רשימת מחרוזות למחרוזת אחת](https://vsavkin.com/modifying-a-list-of-strings-into-a-single-string-in-elm-a9dcae082f20)
- [פונקציה `String.map`](https://package.elm-lang.org/packages/elm/core/latest/String#map)