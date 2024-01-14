---
title:                "Haskell: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה?

ב-Haskell, מחרוזות (strings) הן אחת הנתונים הכי שכיחים. כשאנחנו עובדים עם מחרוזות, ייתכן שנרצה להחליף אותיות גדולות בקטנות או להפוך את כל המחרוזת לאותיות קטנות. כך, נקבל מחרוזת חדשה באותה רמת עוצמה כמו המחרוזת המקורית בלי לשנות את הנתונים המקוריים. ייתכן שזה ישתמש כדי להתאים את הפלט שלנו לפורמט מסוים, לטפסים או כל מידע אחר שמצריך טיפול במחרוזות באותה פורמט.

## איך לעשות זאת?

כדי להמיר את מחרוזת לאותיות קטנות ב-Haskell, נשתמש בפונקציה `toLower` שמצויה במודול `Data.Char`. נתחיל על ידי טעינת המודול בעזרת הפקודה `import Data.Char`. לאחר מכן, נשתמש בפונקציה `toLower` על מחרוזת המקור:

```Haskell
import Data.Char

toLower "HELLO WORLD" -- פלט: "hello world"
toLower "haskell" -- פלט: "haskell"
```

ניתן גם להשתמש בפונקציה `map` כדי להחיל את השינוי לכל אות במחרוזת:

```Haskell
import Data.Char

map toLower "HASKELL" -- פלט: "haskell"
```

כמו כן, במקום להשתמש במודול `Data.Char`, ניתן גם ליצור פונקציה מותאמת אישית שתעשה זאת עבורנו:

```Haskell
lowerString :: String -> String
lowerString = map toLower

lowerString "FUNKY" -- פלט: "funky"
```

## חקירה מעמיקה

כרגע, כשאנחנו משתמשים בפונקציה `toLower`, היא באמת ממירה את כל החרות לאותיות קטנות לפי הקוד ASCII. אם ננסה להחיל את הפונקציה על מחרוזת עם תווים שאינם אותיות, כמו מספרים או תווים מיוחדים, היא לא תעשה שום שינוי. לכן, חשוב להיות מוד