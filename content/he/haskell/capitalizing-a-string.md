---
title:                "Haskell: כתיבה מונחצת של מחרוזת"
simple_title:         "כתיבה מונחצת של מחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
יכול להיות ברצון להוסיף תלתל גדול לתווים במחרוזת כדי להדגיש אותם או לתקן את הטור המסויט שלהם.

## כיצד לעשות זאת
ב-Haskell קיימות כמה אפשרויות לכתיבת תלתל גדול לתווים במחרוזת. הנה כמה דוגמאות:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

capitalize "hello world" 
-- Output: "HELLO WORLD"

-- או, כדי לכתוב תלתל גדול רק למילה הראשונה במחרוזת:

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

capitalize "hello world" 
-- Output: "Hello world"
```

## חפירה עמוקה
כדי להבין איך הפונקציה `capitalize` עובדת, נחשוב על הפונקציה `map`. `map` מקבלת פונקציה ורשימה, ויוצרת רשימה חדשה שבה הפונקציה המקורית הוחלה על כל איבר. במקרה שלנו, הפונקציה המקורית היא `toUpper` והרשימה היא כל תווי המחרוזת. כך, הפונקציה `capitalize` מחזירה מחרוזת חדשה שבה כל אות הוחלה תלתל גדול.

## ראה גם
- [החוקים של פונקציות נקיות ב-Haskell](https://medium.com/%E0%A4%AF%E0%A4%82%E0%A4%97-%E0%A4%8F%E0%A4%95-%E0%A4%AC%E0%A4%BE%E0%A4%A4-haskell-%E0%A4%AE%E0%A5%87%E0%A4%82-%E0%A4%AA%E0%A4%B9%E0%A5%87%E0%A4%B2%E0%A5%87-%E0%A4%95%E0%A5%87-%E0%A4%9A%E0%A4%BE%E0%A4%B0%E0%A4%AC-%E0%A4%9B%E0%A4%AA%E0%A4%BE%E0%A4%8F%E0%A4%82-%E0%A4%95%E0%A4%BE%E0%A4%AE%E0%A4%AF%E0%A4%BE%E0%A4%AC%E0%A5%87%E0%A4%A8-f609aea6b478)
- [נסוי קטן עם כתיבת קוד ב-Haskell](https://medium.com/%E0%A4%AF%E0%A4%82%E0%A4%97-%E0%A4%8F%E0%A4%95-%E0%A4%AC%E0%A4%BE%E0%A4%A4-haskell-%E0%A4%AE%E0%A5%87%E0%A4%82-%E0%A4%AA%E0%A4%B9%E0%A5%87%E0%A4%B2%E0%A5%87-%E