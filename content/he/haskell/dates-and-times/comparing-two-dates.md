---
date: 2024-01-20 17:33:07.221281-07:00
description: "How to (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05DC\u05D4\
  \u05E9\u05D5\u05D5\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-Haskell\
  \ \u05DC\u05D0 \u05EA\u05DE\u05D9\u05D3 \u05D4\u05D9\u05D4 \u05E4\u05E9\u05D5\u05D8\
  . \u05D1\u05E2\u05D1\u05E8, \u05E0\u05D3\u05E8\u05E9\u05D5 \u05E1\u05E4\u05E8\u05D9\
  \u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D5\u05EA \u05D0\u05D5 \u05E7\
  \u05D5\u05D3 \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC. \u05D4\u05D9\u05D5\u05DD, \u05E2\
  \u05DD `Data.Time`, \u05D6\u05D4 \u05D9\u05E9\u05E8 \u05D5\u05E0\u05D5\u05D7. \u05D0\
  \u05DC\u05D8\u05E8\u05E0\u05D8\u05D9\u05D1\u05D5\u05EA\u2026"
lastmod: '2024-04-05T22:50:53.595563-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05E0\u05D3\u05E8\u05E9\u05D5 \u05E1\u05E4\u05E8\
  \u05D9\u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D5\u05EA \u05D0\u05D5\
  \ \u05E7\u05D5\u05D3 \u05DE\u05E1\u05D5\u05E8\u05D1\u05DC."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

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
