---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- /he/haskell/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:40.902439-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
קביעת תאריך בעתיד או העבר זה לחשב מועד שאחרי או לפני נקודת זמן מסוימת. תכנותים עושים את זה לתזמון אירועים, להזכיר תקופות אחריות, ועוד.

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
