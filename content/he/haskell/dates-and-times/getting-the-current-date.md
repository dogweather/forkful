---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:02.919322-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 \u05E9\u05DC Haskell,\
  \ `base`, \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA \u05D4\u05DE\u05D5\u05D3\u05D5\
  \u05DC `Data.Time` \u05E9\u05DE\u05E6\u05D9\u05E2 \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05DC\u05E2\u05D1\u05D5\u05D3\u05D4\
  \ \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D6\u05DE\u05E0\
  \u05D9\u05DD. \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05DC\u05D4\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.430517-06:00'
model: gpt-4-0125-preview
summary: "\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8 \u05E9\u05DC Haskell, `base`, \u05DE\u05E1\u05E4\u05E7\u05EA \u05D0\u05EA\
  \ \u05D4\u05DE\u05D5\u05D3\u05D5\u05DC `Data.Time` \u05E9\u05DE\u05E6\u05D9\u05E2\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05DC\
  \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D5\u05D6\u05DE\u05E0\u05D9\u05DD."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות:
ספריית הסטנדרט של Haskell, `base`, מספקת את המודול `Data.Time` שמציע פונקציונליות לעבודה עם תאריכים וזמנים. הנה איך להשתמש בו כדי לקבל את התאריך הנוכחי:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

דוגמת פלט:
```
2023-04-12
```

ליותר גמישות, כמו עיצוב התאריך או עבודה עם אזורי זמן שונים, הספרייה `time` היא בלתי נפרדת. הנה איך אפשר לעצב את התאריך הנוכחי:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

זה מדפיס את התאריך הנוכחי בתבנית `YYYY-MM-DD`, מותאם לאזור הזמן המקומי.

בנוסף, לתמיכה בספריות של גורמים שלישיים, `time` מומלצת מאוד ונעשה בה שימוש נרחב בקרב קהילת Haskell בשל יכולותיו הרחבות למניפולציה של תאריכים וזמנים. הדוגמאות למעלה משתמשות בספרייה זו.

אם אתה צריך ממשיכות מורכבת יותר של מניפולציית תאריכים, כולל פרסור ממחרוזות או פעולות אריתמטיות עם תאריכים וזמנים, חקירת פונקציות נוספות בתוך `Data.Time` תהיה מועילה.
