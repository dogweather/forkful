---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות הוא התהליך שבו אנו מבצעים שינוי של כל האותיות הגדולות במחרוזת לאותיות קטנות. תכנתים משתמשים בזה בעיקר למטרות דמה, כאשר נדרשת אחידות של הנתונים.

## איך לעשות:
```Haskell
import Data.Char (toLower)

lowercaseStr :: String -> String
lowercaseStr = map toLower
```
דוגמת פלט:
```Haskell
lowercaseStr "Hello, World!"
-- "hello, world!"
```

## צלילה עמוקה:
ישנן הרבה שפות תכנות שבהן ניתן להמיר מחרוזת לאותיות קטנות, אך ב-Haskell זה מתבצע באמצעות הפונקציה `toLower` שנמצאת במודול `Data.Char`. יתרה מכך, תכלית הפונקציה `map` היא להחיל פעולה (כמו `toLower`) על כל איבר ברשימה (או במקרה שלנו, מחרוזת).

למרות שהפונקציה `toLower` היא אחת האפשרויות, אפשר גם להשתמש בקוד שנכתב בהשם עצמו, שימשיך לעבוד גם אם ה-API של `Data.Char` ישתנה עתידית. למשל, במקרה שבו יש מחרוזת של ASCII, ניתן לשנות כל אות גדולה לאות קטנה באמצעות הוספה של 32 לקוד ה-ASCII שלה.

```Haskell
lowercaseAscii :: String -> String
lowercaseAscii = map (\c -> if 'A' <= c && c <= 'Z' then toEnum (fromEnum c + 32) else c)
```

## ראה גם:
1. [Hoogle Documentation - `toLower`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toLower): מסמך ה-API של `toLower` במודול `Data.Char`.
2. [Haskell Wiki - "Lists"](https://wiki.haskell.org/Lists): מידע נוסף על איך עובד `map`.
3. [ASCII Table](http://www.asciitable.com/): מידע נוסף על קודי ASCII ואיך הם קשורים לאותיות גדולות וקטנות.