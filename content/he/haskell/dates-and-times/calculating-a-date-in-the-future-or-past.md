---
date: 2024-01-20 17:31:40.902439-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Haskell, \u05D7\
  \u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\u05EA\u05D9\
  \u05D3 \u05D0\u05D5 \u05D4\u05E2\u05D1\u05E8 \u05DE\u05EA\u05D1\u05E6\u05E2 \u05D1\
  \u05E2\u05D6\u05E8\u05EA \u05D4-library `Data.Time`, \u05E9\u05DE\u05E1\u05E4\u05E7\
  \u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DC\u05D8\u05D9\u05E4\
  \u05D5\u05DC \u05D1\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D1\u05D6\
  \u05DE\u05E0\u05D9\u05DD. \u05DC\u05E4\u05E0\u05D9 \u05DE\u05E6\u05D9\u05D0\u05EA\
  \ \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05DB\u05D0\u05DC\u05D5,\u2026"
lastmod: '2024-04-05T22:50:53.596734-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Haskell, \u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D1\u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D4\u05E2\u05D1\u05E8 \u05DE\u05EA\
  \u05D1\u05E6\u05E2 \u05D1\u05E2\u05D6\u05E8\u05EA \u05D4-library `Data.Time`, \u05E9\
  \u05DE\u05E1\u05E4\u05E7\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA\
  \ \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D5\u05D1\u05D6\u05DE\u05E0\u05D9\u05DD."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```Haskell
import Data.Time

-- הוספת ימים לתאריך נתון
addDaysToCurrentDate :: Integer -> IO ()
addDaysToCurrentDate days = do
  today <- getCurrentTime
  let futureDate = addDays days (utctDay today)
  putStrLn $ "התאריך בעוד " ++ show days ++ " ימים יהיה: " ++ show futureDate

-- ניצול הפונקציה
main :: IO ()
main = addDaysToCurrentDate 10
```

פלט דוגמא:
```
התאריך בעוד 10 ימים יהיה: 2023-04-23
```

## צלילה לעומק:
ב-Haskell, חישוב תאריך בעתיד או העבר מתבצע בעזרת ה-library `Data.Time`, שמספקת פונקציות לטיפול בתאריכים ובזמנים. לפני מציאת ספריות כאלו, תכנותים נאלצו לטפל בעצמם בסיבוכיות של לוחות שנה וליקויים. חלופות כוללות שימוש בפונקציות של המערכת הפעלה או ספריות צד-שלישי. חשוב לזכור שתאריכים ושעות הם רגישים לאזור זמן ולשינויים של שעון קיץ/חורף.

## ראו גם:
- המדריך הרשמי לספרית [`Data.Time`](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html)
- תיעוד Haskell על עבודה עם Time Zones: [https://hackage.haskell.org/package/time-1.12/docs/Data-Time-LocalTime.html](https://hackage.haskell.org/package/time-1.12/docs/Data-Time-LocalTime.html)
