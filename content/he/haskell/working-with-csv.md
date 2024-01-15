---
title:                "עבודה עם קובץ csv"
html_title:           "Haskell: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

ניתן לעבוד עם קבצי CSV בקלות בשפת Haskell. מכיוון שקבצי CSV הם פופולריים בכל עולם התכנות ובעסקים, ידע בסיסי בעבודה עם CSV יכול להיות מאוד מועיל והכרחי למתכנתים.

## איך לעבוד עם CSV בשפת Haskell

הפעולה הראשונה שצריך לעשות היא לייבא את מודול ה-CSV של הספריה המובנית Data.Csv. לעבוד עם קבצי CSV, ניתן להשתמש בפונקציות כמו encode ו-decode. לדוגמה, ניתן לקרוא קובץ CSV עם הקוד הבא:

```Haskell
import Data.Csv

main :: IO ()
main = do
  csvFile <- readFile "file.csv"
  csvData <- decode NoHeader csvFile :: Either String (Vector (Vector String))
  case csvData of
    Left err -> putStrLn "לא ניתן היה לקרוא את הקובץ:"
    Right v -> print v
```

תוצאת הקוד הנ"ל תהיה משתנה מסוג Either הכולל או רשימת כל השגיאות שהתקשרו בזמן קריאת הקובץ, או מהנתונים שנקראו בצורה נכונה במיקום Right.

## נחישות

אם תצטרכו לעבוד עם קבצים CSV בקוד הגדול יותר, כדאי להשתמש במודול Data.Csv ולימוד פונקציות נוספות כמו decodeNamed ו-parse. בנוסף, ניתן לתאר קבצי CSV עם סדר עמודות ללא שם באמצעות התאמת סוגי נתונים באמצעות המחלקה FromNamedRecord.

## ראו גם

- [התעסקות עם קבצי CSV ב-Haskell](https://www.schoolofhaskell.com/user/Uploads/files/CSV%20in%20Haskell.pdf)
- [תיעוד מפורט של Data.Csv ב-Haskell](https://hackage.haskell.org/package/csv/docs/Data-Csv.html)