---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
שינוי מחרוזת לאותיות קטנות הוא לשנות את האותיות הגדולות במחרוזת לקטנות. תכנתים עושים את זה כדי להשוות בקלות בין מחרוזות, מבלי לדאוג לדיכוי.

## איך לעשות:
ב- Elm, ניתן לשנות מחרוזת לאותיות קטנות באמצעות הפונקציה `String.toLower`. שימוש בפונקציה זו נראה כך:

```Elm
import String

txt = "HELLO WORLD"
lowerTxt = String.toLower txt

-- הדפסה של lowerTxt תביא ל- "hello world"
```

## צלילה עמוקה
שינוי מחרוזת לאותיות קטנות הוא רעיון תכנותי ישן שראה אור בשנות ה-60. ב-Elm, חייבים להשתמש במודול `String` שמכיל את הפונקציה `toLower`. אפשר לבחור לבצע את המרה באמצעות קוד נוסף, אך מומלץ להשתמש בכלי התכנות הסטנדרטיים של שפת התכנות.

## ראו גם
אתר הבית של Elm: [אפשר למצוא כאן](https://elm-lang.org/), הדריך הרשמי: [מדריך Elm](https://guide.elm-lang.org/), מודול ה-String של Elm: [String Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toLower).