---
title:    "Haskell: קבלת תאריך נוכחי"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, השתמשות בתאריך הנוכחי היא חלק בלתי נפרד מתהליך הפיתוח. דבר זה יכול להיות שימושי במגוון רחב של תכניות, כולל אפליקציות ואתרים תדירים, וכן לפתרונות לכספים וניתוח המידע המודרני.

## איך לעשות זאת

בשפת התכנות המעבדת Haskell, קל מאוד לקבל את התאריך הנוכחי באמצעות שימוש בפונקציית `getCurrentTime` מחבילת `Data.Time`:

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    print now

-- 2021-10-19 17:59:32.12345678 UTC
```

ישנם כמה פונקציות נוספות כמו `getCurrentTime` המציעות אפשרויות נוספות לעיבוד וייצוג של התאריך הנוכחי בפורמט שונה, כמו `getZonedTime` ו- `getUTCTime`.

## לחפש עמקים נמוכים

לאחר שלמדנו איך לקבל את התאריך הנוכחי בשפת Haskell, ניתן לעשות עוד ניסיונות עם המידע שקיבלנו. לדוגמה, ניתן לשנות את פורמט התאריך על ידי שימוש בפונקציות כמו `formatTime` ו- `parseTimeM`. ניתן גם לבצע חישובים וניתוחים מתקדמים על התאריך כדי לנתח את המידע המדויק של הזמן.

## ראה גם

* [פיקודי התאריך והזמן ב-Haskell](https://www.haskell.org/ghc/blog/20120304-time.html)
* [מדריך גישה נעימה לזמן ב-Haskell](https://orbifold.xyz/getting-time-right-the-endgoal.html)
* [מדריך לתמיכה בתכונות זמן של Swift ב-Haskell](https://github.com/tonarino/asa-time)