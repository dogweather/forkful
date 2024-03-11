---
date: 2024-01-20 17:45:59.377974-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA (substrings) \u05D1-Haskell \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\
  \u05D1\u05D7\u05D5\u05E8 \u05D7\u05DC\u05E7 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA. \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DC\u05E2\u05D9\
  \u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8, \u05D5\u05DC\u05D9\u05D3\u05D5\u05D0\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05DE\u05E9\u05D9\u05DE\
  \u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05E9\u05D3\u05D5\u05E8\u05E9\u05D5\
  \u05EA \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD\u2026"
lastmod: '2024-03-11T00:14:12.848180-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA (substrings) \u05D1-Haskell \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05E0\u05D5 \u05DC\u05D1\
  \u05D7\u05D5\u05E8 \u05D7\u05DC\u05E7 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA. \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DC\u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8, \u05D5\u05DC\u05D9\u05D3\u05D5\u05D0 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05D0\u05D5 \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\
  \u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05E9\u05D3\u05D5\u05E8\u05E9\u05D5\u05EA\
  \ \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD\u2026"
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות (substrings) ב-Haskell הוא פעולה שמאפשרת לנו לבחור חלק מתוך מחרוזת. זה נעשה לעיבוד טקסט, ולידוא נתונים או למשימות אחרות שדורשות עבודה עם מידע מובנה.

## איך לעשות:
```Haskell
import Data.Text (Text)
import qualified Data.Text as T

-- חילוץ תת-מחרוזת ממיקום התחלתי עד סוף
substringFrom :: Int -> Text -> Text
substringFrom start text = T.drop start text

-- חילוץ תת-מחרוזת מהתחלה ועד מיקום סופי
substringTo :: Int -> Text -> Text
substringTo end text = T.take end text

-- חילוץ תת-מחרוזת בעזרת טווח
substring :: Int -> Int -> Text -> Text
substring start end text = T.take (end - start) (T.drop start text)

-- דוגמה לשימוש
main :: IO ()
main = do
  let text = T.pack "שלום, עולם!"
  putStrLn . T.unpack $ substringFrom 6 text  -- "עולם!"
  putStrLn . T.unpack $ substringTo 5 text    -- "שלום"
  putStrLn . T.unpack $ substring 7 12 text  -- "עולם"
```
תוצאת הקוד: ביצוע פונקציות החילוץ על המחרוזת "שלום, עולם!" יחזיר "עולם!", "שלום", ו"עולם" בהתאמה.

## צלילה עמוקה
Haskell משתמש בפונקציות חילוץ מחרוזות מהספריה `Data.Text` כדי להימנע מעבודה עם מחרוזות מטיפוס `String`, שהן פחות יעילות. ההיסטוריה של פונקציות החילוץ הופכת את עבודת הטקסט לפשוטה יותר ב-Haskell. ישנם גם שיטות אלטרנטיביות כמו פרסור (parsing) וביטויים רגולריים, אך לעיתים הפונקציות המובנות עושות את העבודה נאמנה ובמהירות. דבר נוסף שחשוב לדעת הוא שחילוץ תת-מחרוזות צריך להיעשות תוך כדי התחשבות במיקומים שמאחרי הגבולות שאתה מגדיר, כדי להימנע משגיאות זמן ריצה.

## ראה גם
- ספריית המחרוזות `Data.Text`: http://hackage.haskell.org/package/text
- מבוא לשפת Haskell (באנגלית): http://learnyouahaskell.com/chapters
- מדריך לביטויים רגולריים ב-Haskell: https://wiki.haskell.org/Regular_expressions
