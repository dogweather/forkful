---
date: 2024-01-26 01:08:29.395591-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05DC\u05DE\u05E2\u05E9\u05D4 \u05D4\u05E9\u05D0\u05E8\u05EA\
  \ \u05DE\u05E1\u05DC\u05D5\u05DC \u05E9\u05DC \u05E4\u05D9\u05E8\u05D5\u05E8\u05D9\
  \ \u05DC\u05D7\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD \u05D0\u05D5 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\
  \u05EA\u05D5\u05E2\u05D3\u05D9\u05DD, \u05D0\u05E9\u05E8 \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\
  \u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DE\u05D4 \u05E9\u05D4\u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u05E2\u05D5\u05E9\u05D4\
  \ \u05D1\u05DB\u05DC \u05E8\u05D2\u05E2\u2026"
lastmod: '2024-03-13T22:44:39.423952-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA\
  \ \u05D4\u05D5\u05D0 \u05DC\u05DE\u05E2\u05E9\u05D4 \u05D4\u05E9\u05D0\u05E8\u05EA\
  \ \u05DE\u05E1\u05DC\u05D5\u05DC \u05E9\u05DC \u05E4\u05D9\u05E8\u05D5\u05E8\u05D9\
  \ \u05DC\u05D7\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D0\u05D9\u05E8\
  \u05D5\u05E2\u05D9\u05DD \u05D0\u05D5 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05DE\
  \u05EA\u05D5\u05E2\u05D3\u05D9\u05DD, \u05D0\u05E9\u05E8 \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\
  \u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05DE\u05D4 \u05E9\u05D4\u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DC\u05DA \u05E2\u05D5\u05E9\u05D4\
  \ \u05D1\u05DB\u05DC \u05E8\u05D2\u05E2\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
weight: 17
---

## מה ולמה?
לוגינג בתכנות הוא למעשה השארת מסלול של פירורי לחם בצורה של אירועים או הודעות מתועדים, אשר ניתן להשתמש בהם כדי לעקוב אחר מה שהאפליקציה שלך עושה בכל רגע נתון. מתכנתים עושים זאת כדי לאבחן בעיות, לנטר את ביצועי המערכת ולבדוק התנהגות לצורכי אבטחה וציות.

## איך לעשות:
בהאסקל, ניתן לממש לוגינג באמצעות ספריות כמו `monad-logger` או `hslogger`. הנה דוגמה מהירה באמצעות `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "מתחילים את האפליקציה..."
    liftIO $ putStrLn "עושים עבודה קריטית..."
    logErrorN "אופס! משהו השתבש."

main :: IO ()
main = runStdoutLoggingT logExample

{- פלט לדוגמה
[Info] מתחילים את האפליקציה...
עושים עבודה קריטית...
[Error] אופס! משהו השתבש.
-}
```

דוגמה פשוטה זו מדגימה איך ניתן לפזר הודעות לוג ברחבי הקוד שלך כדי לקבל תובנות על מה שקורה בזמן ריצה. `logInfoN` ו-`logErrorN` משמשים לתיעוד הודעות מידע ושגיאה בהתאמה.

## צלילה עמוקה:
לוגינג התפתח מהרבה מעבר לפקודות הדפסה פשוטות למסגרות לוגינג מתוחכמות. בעבר, לוגים היו רק פלטים טקסטואליים לקונסולה או לקובץ, אך כעת הם כוללים נתונים מובנים שניתן לנתח ולפרש באמצעות כלים שונים.

בהאסקל, ניתן לבצע לוגינג בסגנון פונקציונלי טהור שכרוך במסירה מפורשת של פעולות לוג או באמצעות הקשרים מונדיים לחוסר טיהור, שם הלוגרים משולבים בצורה לא מובהקת דרך החישוב.

לדוגמה, הספריה `hslogger` היא יותר מסורתית ומתחלפת בהשוואה ל-`monad-logger`. `monad-logger` מציעה אינטגרציה עם מחסנית המונדות ומספקת גמישות רבה יותר במונחים של עיצוב הפלט ושליטה. שתי הספריות מאפשרות לך להגדיר רמות לוג, המסייעות בסינון הודעות לוג בהתאם לחשיבותן. רמות הלוג כוללות דיבאג, מידע, התראה, אזהרה, שגיאה, קריטי, אזעקה וחירום.

הגישה של האסקל ללוגינג לעיתים קרובות מתיישבת עם המיקוד שלה בבטיחות טיפוסית וטיהור. ניתן לטפל בלוגים בצורה שאפילו אם הלוגינג נכשל, זה לא יגרום לקריסת האפליקציה הראשית בשל יכולות ההתמודדות עם שגיאות האמינות של האסקל.

## ראה גם:
- [תיעוד של `monad-logger` ב-Hackage](https://hackage.haskell.org/package/monad-logger)
- [חבילת `hslogger` ב-Hackage](https://hackage.haskell.org/package/hslogger)
- [האסקל המעשי, פרק 19, על טיפול בשגיאות](http://book.realworldhaskell.org/read/error-handling.html)
- [ממשק הלוגינג של האסקל (log-base)](https://hackage.haskell.org/package/log-base)
