---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?
המרת תאריך למחרוזת היא תהליך בו מומר תאריך למחרוזת המייצגת אותו. מתכנתים מבצעים זאת כדי להציג תאריכים בצורה קריאה להושט למשתמש.

# איך ל:
כאן כמה דוגמאות להמרת תאריך למחרוזת בעזרת פונקציות של Haskell:
```Haskell
import Data.Time.Format (formatTime, defaultTimeLocale)

let date = UTCTime (fromGregorian 2022 3 20) (secondsToDiffTime 0)
formatTime defaultTimeLocale "%B %d, %Y" date
```
פלט:
```Haskell
"March 20, 2022"
```
בדוגמה זו, השתמשתי בפונקציות `formatTime` ו-`defaultTimeLocale` מתוך המודול `Data.Time.Format` ובפונקציות `UTCTime` ו-`fromGregorian` מתוך `Data.Time.Clock` ו-`Data.Time.Calendar` בהתאמה.

# Deep Dive:
- היסטוריה: במהלך שנים של התפתחות שפות תכנות, נוצרו מגוון שיטות וכלים לטיפול בתאריכים ושעות, כולל המרתם למחרוזות. Haskell הציגה מתכן עגול של פונקציות לתאריך/זמן ב-`Data.Time`.
- חלופות: ישנם מספר דרכים אחרות להמיר תאריך למחרוזת. לדוגמה, היית יכול להשתמש בסיפריית StringTemplate במקום formatTime.
- פרטי ישום: הסיפריה `Data.Time` משתמשת בשיטת התאריך הגרגוריאני, אשר הוטמעה בחומרה רוב מחשבי האיש המודרניים.

# ראה גם:
- [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html) - המסמך הראשי לסיפריה `Data.Time.Format`.
- [https://hackage.haskell.org/package/stringtemplate-0.2.1.1/docs/Text-StringTemplate.html](https://hackage.haskell.org/package/stringtemplate-0.2.1.1/docs/Text-StringTemplate.html) - המסמך הראשי לסיפריה StringTemplate.
- [http://www.haskell.org/ghc/docs/latest/html/libraries/time/Data-Time-Calendar.html](http://www.haskell.org/ghc/docs/latest/html/libraries/time/Data-Time-Calendar.html) - המסמך הראשי מ-GHC עבור `Data.Time.Calendar`.