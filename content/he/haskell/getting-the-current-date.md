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

## מדוע

הכירו את הקוד החדש והמתקדם ביותר בתעשיית התכנות - חסקל  (Haskell)! עם כמה שנים של קיום, הוא כבר הפך לבחירה נפוצה בקרב מתכנתים מתקדמים ומתחילים כאחד. אחד התכונות החדשות המעניינות ביותר של חסקל היא היכולת לקבל את תאריך היום הנוכחי.

במאמר זה נכיר את הרעיון של תאריך נוכחי בחסקל, ונראה כיצד להשתמש בו בקוד שלנו.

## כיצד

תאריך נוכחי בחסקל הוא פינקציה פשוטה שמציגה את התאריך הנוכחי בפורמט מובנה. הנה מספר דוגמאות כיצד להשתמש בפינקציה זו בקוד שלכם:

```Haskell
-- ייבוא מודול לטיפול בתאריכים
import Data.Time

-- הפינקציה המציגה את התאריך הנוכחי
getCurrentDate :: IO String
getCurrentDate = do
  date <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%d/%m/%Y" date
  return formattedDate

-- קריאה לפינקציה והדפסת התוצאה בטקסט
main = do
  date <- getCurrentDate
  putStrLn $ "התאריך הנוכחי הוא: " ++ date
```

הפלט המופיע כולל את התאריך הנוכחי בפורמט נוח וקריא לקריאה על ידי המשתמש.

## Deep Dive

פינקציית התאריך הנוכחי של חסקל משתמשת בפינקציית עזר מובנית בשם `getCurrentTime` שפועלת על מנת להחזיר את התאריך הנוכחי. לאחר מכן, הפינקציה משתמשת בפינקציית עזר נוספת בשם `formatTime`, שמאפשרת לנו לקבל את התאריך בפורמט שנרצה.

הפורמט המוגדר על ידי `formatTime` מכיל כמה תבניות מוגדרות מראש, כגון `%d` ליום ו`%m` לחודש. בנוסף, ניתן גם להשתמש בתבני