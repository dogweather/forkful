---
title:                "Elm: המרת מחרוזת לאותיות קטנות"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

למה לבצע המרת מחרוזת לאותיות קטנות באמצעות תכנות Elm? המרת מחרוזת לאותיות קטנות יכולה להיות שימושית כשאנו עובדים עם נתונים מורכבים באנגלית כמו אימיילים ושמות פרטיים.

## איך לעשות זאת

באמצעות פקודת toLower ואינדקס מחרוזת, ניתן לכתוב פונקציה פשוטה שתמיד תמיר את המחרוזת לאותיות קטנות גם אם יוכנסו לה אותיות גדולות.

```Elm
toLowerExample : String -> String
toLowerExample str =
    String.map toLower str

toLowerExample "ELM" -- Output: "elm"
```

## ידיעה מעמיקה

בתוך פונקצית toLower מעלה פעולתי הממירה את המילה לאותיות קטנות היא לבצע בפקודה String.map toLower המאפשרת לנו לעבור על כל אחת מהתווים שבמחרוזת ולהחיל עליהם פעולה מסוימת, כך שנוכל לשנות את כל האותיות לאותיות קטנות בפעולה אחת.

## ראו גם

- תיעוד רשמי של String.map באתר Elm: https://package.elm-lang.org/packages/elm/core/latest/String#map 
- לומדים לכתוב פונקציות ב-Elm: https://elmprogramming.com/creating-functions.html