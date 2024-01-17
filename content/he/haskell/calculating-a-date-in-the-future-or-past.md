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

חישוב תאריך בעתיד או בעבר הוא תהליך שבו מתכנתים משתמשים בכדי לחשב את התאריך המתאים לנקודת זמן מסוימת בעתיד או בעבר. המוצא של התאריך הזה מועיל ליישומים רבים, כגון תכניות לקראת מועדי משלך, אירועים חשובים, או פשוט כדי לבדוק מתי יום הולדתך הבא. בקיצור, זה מסייע לנו להתמקד במספר תאריכים חשובים לנו מתחת לגיליון הזמן.

## איך לעשות:

תחילה, נתקין את הספרייה Data.Time באמצעות פקודת ההתקנה הבאה:

```Haskell
cabal install time
```

לאחר מכן, נשתמש בפונקציית addDays כדי לחשב תאריך בעתיד או בעבר על ידי הכנסת הודעת התאריך הנוכחי ומספר הימים המבוקש:

``` Haskell
addDays :: Integer -> Day -> Day
```

לדוגמה, נרצה לחשב את התאריך של 100 ימים מעכשיו וניראה כך:

``` Haskell
import Data.Time

main = do
    tz <- getCurrentTimeZone
    today <- getCurrentTime
    let futureDate = addDays 100 (utctDay today)
    putStrLn $ showGregorian $ LocalDate (utcToLocalTime tz today)
    putStrLn $ showGregorian $ LocalDate (utcToLocalTime tz futureDate)
```
ההפלגה הבאה יודפסה:

``` 
100 days from now is 2021-11-07 
and the future date will be 2022-02-10
```

ניתן גם לחשב תאריך בעבר על ידי כניסת מספר שלילי של ימים, כך שנחזיר את התאריך הנמצא לפני תאריך ההתחלה:

``` Haskell
import Data.Time

main = do
    tz <- getCurrentTimeZone
    today <- getCurrentTime
    let pastDate = addDays (-100) (utctDay today)
    putStrLn $ showGregorian $ LocalDate (utcToLocalTime tz today)
    putStrLn $ showGregorian $ LocalDate (utcToLocalTime tz pastDate)
```

ההדפסה הבאה תופיע:

```
100 days ago was 2021-05-28 
and the past date was 2021-02-24
```

## חקירה מעמיקה:

תוכנן חישוב תאריך בעתיד או בעבר לראשונה בשנת 1804 על ידי המתמטיקאי לגראן לוקסט. מאז, התהליך נוצר ושופר באמצעות שפות תכנות רבות, כולל Haskell.

בנוסף, למעבר לפונקציית addDays ישנם עוד כמה אפשרויות לחישוב תאריך בעתיד או בעבר, כמו לחשב תאריך באמצעות שנות, חודשים או פונקציות מתומצתות אחרות מתוך Data.Time ספריית מעבדת.

הספרייה Data.Time היא חלק מפרוייקט Haskell שמטרתו לספק ספריית תארים מתקדמים ויעילים כדי לעזור למתכנתים לנהל תאריכים ושעונים באופן יעיל ונוח.

## ראה גם:

- Data.Time ספריית מעבדת: https://hackage.haskell.org/package/time/docs/Data-Time.html
- ספריית תארים מתקדמים בשפת Haskell: https://haskell.org/haskellwiki/Time_type
- לגארה לוקהסט, מתמטיקאי שתכנן לראשונה את חישוב תאריך בעתיד ובעבר: https://en.wikipedia.org/wiki/Lagrange%27s_formula