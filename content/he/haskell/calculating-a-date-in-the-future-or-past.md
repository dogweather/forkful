---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Haskell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר מתייחס לתהליך של עריכת מספר בימים לתאריך מעובד. מתכנתים מנהלים את התהליך הזה לשלל סיבות, החל מחיררון אירועים לניהול של לוחות זמן.

## כיצד ל:
```Haskell
import Data.Time.Calendar

addDaysToCurrentDate :: Integer -> IO Day
addDaysToCurrentDate n = do
  currentDate <- utctDay <$> getCurrentTime
  return $ addDays n currentDate
```
בהנחה שתרצה להוסיף שלושה ימים לתאריך הנוכחי, פלט דוגמה ייראה משהו כזה:

```Haskell
addDaysToCurrentDate 3 
-- "2022-03-22"
```

## בילועלי
החשיבות של חישוב תאריכים בעבר ובעתיד התבררה בהיסטוריה של מדעי המחשב. לעיתים קרובות, נושאים אחרים כמו חריגת זמנים מאתגרים מאוד, אבל Haskell מספק ספרייה יעילה בשם `Data.Time`. 

כלפיים, ישנן ספריות של שלדי זמן, אך `Data.Time` היא ברירת המחדל של Haskell. ישנם שיקולים לבחור באפשרות זו, החל מהתמיכה שלה בכל סוגי הזמן הדרושים, דרך קלות השימוש שלה, ועד ליכולת להתמודד עם שארית סוגי האירועים.

## ראה גם
* [האנשי העיון של Haskell ל- `Data.Time`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)