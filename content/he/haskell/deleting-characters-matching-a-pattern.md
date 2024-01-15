---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Haskell: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

מחיקת תווים המתאימים לדפוס יכולה להיות מועילה כאשר אנחנו רוצים לקבל מחרוזת חדשה שאינה מכילה את התווים הללו. זה כולל מחיקת רווחים, תווים מיוחדים וכו '.

## איך לעשות זאת

```Haskell
import Data.Text (replace)

-- נמחק את התווים המתאימים לדפוס על ידי החלפתם במחרוזת ריקה
replace "pattern" "" "example string" -- "examp string"

-- אם נרצה למחוק תווים מסוימים בתווים מסוימים נוספים, נוכל להשתמש בפונקציה splitOn כדי לקבל רשימת מחרוזות ולהפעיל את פונקציית replace על כל מחרוזת בנפרד
map (replace "pattern" "") (splitOn " " "hello world") -- ["hello", "world"]

-- למחוק גם תווים מיוחדים נוכל להשתמש בפונקציית filter כדי לבדוק את התווים ולהחזיר רשימת התווים האפשריים
filter (`notElem` "!@#$%") "example!@ string%" -- "example string"
```

## העמקה

כדי למחוק תווים המתאימים לדפוס, אנחנו משתמשים בפונקציית replace מתוך חבילת Data.Text. הפונקציה מחזירה את המחרוזת המקורית עם התווים המתאימים לדפוס נמחקים והתווים הנשמרים מוחזרים. ניתן להשתמש גם בפונקציית filter על מנת לבדוק כיצד התווים מתנהגים ולהחליף אותם בתווים ריקים במקרה הצורך.

## ראו גם

- [מדריך לביצוע פעולות על מחרוזות ב-Haskell](https://haskell.e-bigdata.tech/articles/perform-string-operations-haskell)
- [חבילת Data.Text ב-Haskell](https://hackage.haskell.org/package/text-1.2.3.2/docs/Data-Text.html)