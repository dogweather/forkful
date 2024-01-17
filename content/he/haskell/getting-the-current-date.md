---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Haskell: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

מה ולמה? 

קבלת התאריך הנוכחי היא פעולה שבוצעת כאשר תכנותאנים רוצים להשתמש בתאריך הנוכחי בתוכניותיהם. לדוגמה, כשאנחנו כותבים תאריכים בחלקים שונים של הקוד, נשתמש בפעולה זו כדי להציג את התאריך הנוכחי בתוכניות הריצה שלנו.

איך לעבוד עם הפקודה:

הנה כמה דוגמאות לקוד ולפלט בשפת הפקודות הוסקל:

```Haskell
import Data.Time (getCurrentTime, utcToLocalTime, formatTime, defaultTimeLocale)

-- הפונקציה הבאה מחזירה את התאריך הנוכחי בפורמט מתאים
getCurrentDate :: IO String
getCurrentDate = do
  now <- getCurrentTime
  let localTime = utcToLocalTime defaultTimeLocale now
  return $ formatTime defaultTimeLocale "%a, %d %b %Y" localTime

main = do
  currentDate <- getCurrentDate
  putStrLn $ "התאריך הנוכחי הוא: " ++ currentDate

```

פלט:
```Sh
התאריך הנוכחי הוא: ש' 28 מר 2020
```


עומק נתונים:

יכול להיות שקיבלת התאריך הנוכחי נראת פשוטה, אבל ישנן מספר אפשרויות נוספות למימוש פעולה זו. בעיקר, ניתן להשתמש בספריית נוספת כמו `time`, המציעה פעולות נוספות עבור תאריכים ושעות. בנוסף, ניתן למצוא מידע נוסף על פעולה זו במקורות מקוונים כמו התיעוד הרשמי של הפקודה ובספרים בנושא תכנות ב-Haskell.

ראה גם:

- מדריך לפקודות הוסקל הנוספות: https://www.haskell.org/ghc/docs/latest/html/libraries/

- דוגמאות לשימוש בפקודה: https://www.tutorialspoint.com/haskell/haskell_functions.htm

- ספרייה נוספת לעבודה עם תאריכים: https://hackage.haskell.org/package/time