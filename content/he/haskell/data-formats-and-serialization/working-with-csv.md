---
title:                "עובדים עם CSV"
aliases:
- /he/haskell/working-with-csv/
date:                  2024-02-03T19:20:34.231441-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדי פסיקים) כוללת ניתוח ויצירת קבצים אשר שומרים נתונים טבלאיים בפורמט טקסטואלי פשוט. תכנתים מתעסקים לעיתים קרובות במשימה זו כדי לייבא או לייצא נתונים ביעילות מגליונות אלקטרוניים, מסדי נתונים, או כדי לקדם החלפת נתונים בין תוכנות שונות.

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
