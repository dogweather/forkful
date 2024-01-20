---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV (ערכים מופרדים בפסיקים) נעשית לצורך קריאה וכתיבה של נתונים בפורמט פשוט ונפוץ. תכנתים עושים זאת כדי לשתף נתונים עם מערכות אחרות בקלות ולעבד נתונים בצורה אוניברסלית.

## איך לעשות:
בעבודה עם הספרייה `cassava`, תעזרו בפונקציות `encode` ו`decode` לעיבוד נתונים:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- טיפול בנתונים נכנסים
decodeCSV :: BL.ByteString -> Either String (V.Vector (String, Int, String))
decodeCSV input = decode NoHeader input

-- ייצור נתונים לקובץ CSV
encodeCSV :: V.Vector (String, Int, String) -> BL.ByteString
encodeCSV = encode

-- פלט לדוגמה:
-- נניח קובץ 'data.csv' עם התוכן הבא:
-- name,age,city
-- John,30,New York
-- Jane,25,Boston

-- השימוש בפונקציות
main :: IO ()
main = do
  csvData <- BL.readFile "data.csv"
  case decodeCSV csvData of
    Left err -> putStrLn err
    Right rows -> BL.writeFile "new_data.csv" $ encodeCSV rows
```

## עיון מעמיק:
קבצי CSV נמצאים בשימוש מראשית המחשוב כמעט. נוחותם בגלל פשטותם וקלות הקריאה בעיני ראייה אנושית ובמערכת מחשב. ישנן חלופות רבות, כמו JSON או XML, אך לעיתים CSV הוא הבחירה מסיבות של תאימות או ביצועים. ב-Haskell, `cassava` היא הספרייה הנפוצה ביותר לעבודה עם CSV, מציעה פונקציונליות גמישה והמרה אוטומטית של טיפוסי נתונים.

## ראה גם:
- [Cassava on Hackage](https://hackage.haskell.org/package/cassava)
- [Haskell CSV tutorial](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/)
- [More about CSV on Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)