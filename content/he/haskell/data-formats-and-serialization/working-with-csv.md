---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:34.231441-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D4\u05E1\
  \u05E7\u05DC, \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9 CSV\
  \ \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D2\u05D4 \u05D1\u05D0\u05DE\u05E6\
  \u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 `cassava`, \u05D0\u05D7\
  \u05EA \u05DE\u05D4\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05D4\u05E6\u05D3 \u05E9\
  \u05DC\u05D9\u05E9\u05D9 \u05D4\u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\u05D5\u05EA\
  \ \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5. \u05DC\u05D4\u05DC\u05DF \u05D3\u05D5\
  \u05D2\u05DE\u05D0\u05D5\u05EA \u05D4\u05DE\u05D3\u05D2\u05D9\u05DE\u05D5\u05EA\
  \ \u05D0\u05D9\u05DA \u05DC\u05E7\u05E8\u05D5\u05D0 \u05DE\u05E7\u05D5\u05D1\u05E5\
  \u2026"
lastmod: '2024-03-13T22:44:39.449401-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D4\u05E1\u05E7\u05DC, \u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E7\
  \u05D1\u05E6\u05D9 CSV \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D2\u05D4 \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4\
  \ `cassava`, \u05D0\u05D7\u05EA \u05DE\u05D4\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA\
  \ \u05D4\u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05D4\u05E4\u05D5\u05E4\u05D5\
  \u05DC\u05E8\u05D9\u05D5\u05EA \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

## איך לעשות:
בהסקל, טיפול בקבצי CSV ניתן להשגה באמצעות הספרייה `cassava`, אחת מהספריות הצד שלישי הפופולריות למטרה זו. להלן דוגמאות המדגימות איך לקרוא מקובץ CSV וכיצד לכתוב אליו באמצעות `cassava`.

**1. קריאת קובץ CSV:**

ראשית, וודאו ש-`cassava` מותקן על ידי הוספתו לקובץ ה-cabal של הפרויקט או שימוש ב-Stack.

להלן דוגמה פשוטה לקרוא קובץ CSV ולהדפיס כל רשומה. אנו מניחים כי בקובץ ה-CSV יש שני עמודות: שם וגיל.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " בן " ++ show (age :: Int) ++ " שנים."
```

בהנחה ש-`people.csv` מכיל:
```
John,30
Jane,25
```
הפלט יהיה:
```
John בן 30 שנים.
Jane בת 25 שנים.
```

**2. כתיבת קובץ CSV:**

ליצירת קובץ CSV, ניתן להשתמש בפונקציה `encode` מ-`cassava`.

הנה איך תוכלו לכתוב רשימת רשומות לקובץ CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

לאחר הרצת התוכנית הזו, `output.csv` יכיל:

```
John,30
Jane,25
```

הקדמה מקוצרת זו לעבודה עם קבצי CSV בהסקל באמצעות הספרייה `cassava` מדגימה כיצד לקרוא מקבצי CSV ולכתוב אליהם, מה שהופך משימות של מניפולציה של נתונים לנגישות יותר למי שחדש לשפה.
