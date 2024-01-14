---
title:                "Haskell: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## מדוע

CSV היא פורמט נתונים נפוץ ושימושי שמשמש לאחסון והעברת נתונים בין מערכות שונות. בעזרת לימוד התכנות ב-Haskell, ניתן ליצור קוד איכותי ויעיל לעבוד עם קבצי CSV. בכתבה זו נלמד כיצד ליצור ולעבוד עם קבצי CSV בשפת Haskell כדי לייצר קוד יעיל וקריא.

## כיצד לעבוד עם CSV ב-Haskell

```Haskell
import Text.CSV

-- קריאת קובץ CSV מתוך קובץ קיים בעזרת פונקציה readCSVFromFile
main = do
    csv <- readCSVFromFile "data.csv" --הכנסת שם הקובץ הרצוי
    case csv of
        Left err -> putStrLn $ "קרתה שגיאה בקריאת הקובץ:" ++ err
        Right rows -> print rows --הדפסת כל השורות בטבלה
```

פריטי הנתונים בספרייה הותאמו כלפי המבנה הפנימי של טבלאות CSV. כאן ניתן להתאים את הקוד בהתאם למבנה הנתונים המיוחד של כל קובץ CSV.

## עולם עמוק של עבודה עם CSV

עבודה עם קבצי CSV ב-Haskell יכולה להיות מאתגרת לעתים, אך ניתן ליצור קוד אחיד ויעיל שיעבוד עם כל סוגי הנתונים שנמצאים בשורות ועמודות שונים בטבלה. ישנם ספריות נוספות ב-Haskell שיכולות לעזור בעבודה עם קבצי CSV, כגון הספרייה «hcsv», שמאפשרת יצירה וקריאה של קבצים עם מבנים מורכבים יותר.

## ראו גם

* ספריית hcsv: https://hackage.haskell.org/package/hcsv
* המדריכים של Hoogle: https://www.haskell.org/hoogle/