---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# מה זה ולמה? 
חילופי תת-מחרוזות הם הפעולה של חילופי רצפים של תווים מתוך מחרוזת. מתכנתים עושים זאת כדי למנות, להבחין או לעבד נתונים מסוימים.

# איך לעשות: 
```Haskell
import Data.List

-- ניצול substring באמצעות drop ו take
substring :: Int -> Int -> String -> String
substring startPos endPos = take (endPos - startPos + 1) . drop startPos
```
יציאה לדוגמה:
```Haskell
main = do
    putStrLn $ substring 0 4 "היי, עולם של Haskell"
    -- יוצא "היי,"
```

# צלילה עמוקה:
1. הHaskell, בניגוד לשפות תכנות אחרות שהגדירו מונחים מוכנים מראש למחרוזת, מספק בפועל דרכים מרכזיות לתיחום תת-מחרוזות: `drop` ו `take`.
2. חלופות לגישה זו כוללות שימוש בספריות נוספות, כמו Data.ByteString.Char8 או Data.Text, שמספקות דרכים מהירות ויעילות יותר לעבוד עם מחרוזות ותת-מחרוזות.
3. במימוש זה, `drop` "מניח" את המחרוזת עד שהתווים מתחילים ו `take` מחזיר מחרוזת שארכה היא המרחק בין נקודת ההתחלה לנקודת הסיום.

# ראה גם:
- Data.ByteString.Char8: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString-Char8.html
- Data.Text: http://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html
- פונקציות מחרוזת Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/string