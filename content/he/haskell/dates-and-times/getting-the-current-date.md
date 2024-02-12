---
title:                "קבלת התאריך הנוכחי"
aliases: - /he/haskell/getting-the-current-date.md
date:                  2024-02-03T19:10:02.919322-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
אחזור התאריך הנוכחי ב-Haskell כולל את השגת הזמן הנוכחי של המערכת והפיכתו לתבנית תאריך קריאה. מתכנתים עושים זאת על מנת לבצע פעולות המבוססות על התאריך, כגון רישום לוגים, תזמון משימות, או חותמת זמן לאירועים ביישומים.

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
