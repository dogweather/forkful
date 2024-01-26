---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:07.221281-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
להשוות שתי תאריכים זה לבדוק מי מהם קדם לשני או אם הם זהים. תכנתים עושים את זה לסדר אירועים כרונולוגית, לחשב משך זמן, ולהפעיל פונקציות בזמנים מסוימים.

## How to (איך לעשות:)
```Haskell
import Data.Time

-- | Compare two dates and print the result
compareDates :: IO ()
compareDates = do
    -- Define two dates
    let date1 = fromGregorian 2023 3 15  -- 15th March 2023
    let date2 = fromGregorian 2024 3 16  -- 16th March 2024

    -- Comparing dates
    case compare date1 date2 of
        LT -> putStrLn "First date is earlier."
        EQ -> putStrLn "Dates are equal."
        GT -> putStrLn "First date is later."
```
פלט:
```
First date is earlier.
```
## Deep Dive (עומק השקעה)
להשוות תאריכים ב-Haskell לא תמיד היה פשוט. בעבר, נדרשו ספריות חיצוניות או קוד מסורבל. היום, עם `Data.Time`, זה ישר ונוח.

אלטרנטיבות קיימות. מתכנתים יכולים להשתמש בפונקציות כמו `diffUTCTime` לחישוב הפרשים בשניות ולטפל בזמן גם באזורים זמניים שונים.

בפנים, `Data.Time` מתמקד בשימוש נכון בטיפוסים וספריות סטנדרטיות של Haskell. זה מגביר את בטיחות הטיפוס ומפחית האפשרות לבאגים קשורים לתאריכים.

## See Also (ראו גם)
- Haskell Documentation for the `time` package: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
