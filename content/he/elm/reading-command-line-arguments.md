---
title:                "Elm: קריאת ארגומנטים בשורת הפקודה"
simple_title:         "קריאת ארגומנטים בשורת הפקודה"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

קריאת ארגומנטים בשורת הפקודה היא כלי חשוב בתכנות ב-Elm שיכול להעצים את יכולות התוכנה שלכם. כתיבת תוכניות שישתמשו בארגומנטים בשורת הפקודה יכולה להפוך את התוכניות שלכם לנמכות יותר וחכמות יותר, תוך שימוש בקלות בשורת הפקודה.

## איך לעשות זאת

```Elm
import Html exposing (text)
import Platform.Cmd exposing (args)

main =
    Html.text (args)
```
```
elmi
```
```
| "Hello"
```

הדוגמא הבאה מראה כיצד לכתוב קוד ב-Elm שיקרא את המשתנה `args` באמצעות פונקציות מובנות של הפלטפורמה. פלט התוכנית הינו הרשימה של תפוצות המשתנים שהועברו לשורת הפקודה.

## להעמיק

כאשר מעוניינים לקרוא את הפרמטרים שהועברו לתכנית בשורת הפקודה, ניתן לעשות זאת על ידי כתיבת קוד שיקרא מערך המשתנים `argv`, או ליצור קובץ מיוחד שמכיל את הפרמטרים או להשתמש בתוכנית חיצונית שתמצא את הפרמטרים לכם.

## ראו גם

- [תיעוד רשמי של שורת הפקודה של Elm](https://guide.elm-lang.org/interop/cmd.html)
- [מדריך על כתיבת תוכניות בשורת הפקודה עם Elm](https://medium.com/@preethi_a/writing-command-line-tools-using-elm-c2bf4ac42a7a)
- [פרויקט בגיטהאב של ניידת כיוונים בשורת הפקודה ב-Elm](https://github.com/ababkin/elm-cli-zygohora)