---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:36:36.332673-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך ממחרוזת היא תהליך שבו הופכים תיאור תאריך בתור מחרוזת למבנה נתונים שניתן לעבוד איתו בקוד. מתכנתים עושים זאת כדי לאפשר ניתוח ותפעול של תאריכים באופן יעיל ונוח.

## איך לעשות:
בואו נראה כיצד אפשר לעשות את זה בHaskell.

```Haskell
import Data.Time.Format
import Data.Time.Clock
import Locale

parseDate :: String -> Maybe UTCTime
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" str

main :: IO ()
main = print $ parseDate "2023-03-14 08:44:55"
```

פלט לדוגמה:

```
Just 2023-03-14 08:44:55 UTC
```

הפונקציה `parseDate` ממירה מחרוזת לתאריך ושעה בUTC. `parseTimeM` משמשת לביצוע ההמרה, עם פורמט מוסכם עולמית.

## טבילה עמוקה
ההסטוריה של ניתוח תאריכים חוזרת לימי התחלה של התוכנות, כאשר היה צורך לייצר ממשק קליל למשתמשים להקליד תאריכים. בHaskell, ספריית Time מספקת מגוון רחב של כלים לעבוד עם תאריכים וזמנים, שמגיעים במקום פונקציות ידניות מסורבלות שהיו פופולריות בעבר.

חלופות קיימות כגון הספרייה `time` דורשת למעשה ידע מסוימת בפורמט התאריך של המחרוזת. בימינו, יש מגוון ספריות שמספקות פונקציונליות דומה עם ניתוח יותר חופשי של פורמטים.

בתוך הפונקציה `parseTimeM`, המרה מוצלחת תחזיר `Just` עם הערך, בעוד שתאריך לא תקין יחזיר `Nothing`, שזה מאפשר לטפל בשגיאות בקלות.

## ראו גם
1. הספרייה `time` עם התיעוד הרשמי: [Time Library on Hackage](https://hackage.haskell.org/package/time)
2. מדריך לניתוח תאריכים בHaskell: [Learn You a Haskell - Dates and Times](http://learnyouahaskell.com/input-and-output#dates-and-times)
3. פוסט בבלוג על טיפול בתאריכים וזמנים בHaskell: [Working with Dates and Times in Haskell](https://two-wrongs.com/haskell-time-library-tutorial)
