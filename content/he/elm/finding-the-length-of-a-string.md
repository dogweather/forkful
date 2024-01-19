---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
מציאת אורך של מחרוזת היא תהליך של ספירת התווים המרכיבים אותה. מתכנתים מבצעים זאת למגוון סיבות, כולל ולידציה של קלט, עיבוד נתונים ועוד.

## איך לעשות:
```Elm
import String

-- דוגמא לקוד המחזיר את אורך של מחרוזת
lengthOfString : String -> Int
lengthOfString str = String.length str

-- דוגמא לשימוש בפונקציה
main =
    let
        str = "שלום עולם"
    in
    Text (String.fromInt (lengthOfString str))
```
#### תוצאה:
```
9
```
## בעומק:
**הקשר ההיסטורי**: בשפת Elm, הפונקציה `String.length` הייתה מוכרחת להתגבר על מגבלות של כך שמחרוזת הייתה תכליתית. עם הזמן, היעילות שלה נותרה יציבה, ונוסף לה אהבה מהקהל.

**חלופות**: במגרעת היא מאפשרת לנסות לממש פונקציה כזו בהשוואה לתווים אחד אחרי השני.

**פרטי ביצוע**: הפונקציה `String.length` מסתמכת על הפונקציה `Array.map` שמספרת את מספר התווים במחרוזת. היא אינה נמשכת לאורך תווים 'ריקים'.

## ראה גם:
[דוקומנטציה של Elm על מחרוזות](https://package.elm-lang.org/packages/elm/core/latest/String)
[מדריך למתחילים בשפת Elm](https://guide.elm-lang.org/)