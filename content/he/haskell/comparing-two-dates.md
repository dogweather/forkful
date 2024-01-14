---
title:    "Haskell: להשוואת שתי תאריכים"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## מדוע

תאריך וזמן הם שני משתנים חשובים ביותר בכל תוכנית תוכנה. בהתבוננות יותר עמוקה, ניתן לקבל נתונים נוספים ומועילים על התאריכים שמשתמשים בהם בתכנית הפיתוח. בפוסט זה נלמד כיצד להשתמש בחשבון של שני תאריכים כדי להשוות ביניהם ולדעת מי מהם הוא יותר חדש.

## איך לעשות

עבודה עם תאריכים בשפת התכנות הפונקציונלית Haskell נעשת באמצעות המודול Data.Time המאפשר ליצור ולעבד תאריכים וזמנים בקלות. נתחיל עם קוד פשוט המייצג שני תאריכים בפורמט של Date, Time of Day ו Time Zone:

```Haskell
import Data.Time

date1 = fromGregorian 2020 12 25
time1 = makeTimeOfDay 15 30 0 -- 3:30 PM
timezone1 = utc -- UTC Time Zone
dateTime1 = LocalTime date1 time1

date2 = fromGregorian 2019 1 1
time2 = makeTimeOfDay 12 0 0 -- 12:00 PM
timezone2 = hoursToTimeZone (-5) -- Eastern Time Zone
dateTime2 = LocalTime date2 time2
```

הכמויות יכולות להיות מייצגות על ידי תאריך וזמן כפונקציות מכוונות, LocalTime עבור DATE, TIME_OF_DAY ו TIME_ZONT שאלה: כאן DateTime1 מייצג את יום חג המולד של שנת 2020, 3:30 PM UTC ו dateTime2 מייצג את 1 בינואר 2019 ב 12:00 PM Eastern Time.

כעת נחשב את ההפרש בין התאריכים בסקריפט הזה על ידי השוואת שני התאריכים עם הודעת מיהו התאריך המוקדם יותר:

```Haskell
compareDates :: LocalTime -> LocalTime -> String
compareDates d1 d2 =
    case compare d1 d2 of
        GT -> "DateTime1 is earlier than DateTime2"
        LT -> "DateTime2 is earlier than DateTime1"
        EQ -> "The dates are the same"

main = do
    putStrLn (compareDates dateTime1 dateTime2) 
```

הפלט המצורף עבור הדוגמה זו הוא:
> DateTime2 is earlier than DateTime1

ניתן להתאים את הפונקציה כדי להחזיר פלט מעודכן יותר, כגון התאריך המדויק המי