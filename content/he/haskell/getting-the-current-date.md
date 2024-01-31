---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:15:11.575044-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי בתכנות מאפשרת לך לטעון ולהשתמש בנתוני זמן חיים כדי להפוך את יישומך לדינמי ורלוונטי. זה חיוני לתפקודים כגון לוגים, תזכורות וכל דבר שתלוי זמן.

## איך לעשות:
כדי לקבל את התאריך הנוכחי ב-Haskell, תצטרך להשתמש בחבילת `time`. תחילה, תצטרך לייבא את המודולים הנכונים:

```Haskell
import Data.Time
```

אז, לקבל את הזמן המקומי הנוכחי:

```Haskell
getCurrentTime :: IO UTCTime
getCurrentTime = getCurrentTime

main :: IO ()
main = do
    now <- getCurrentTime
    print now
```

דוגמא לפלט:

```
2023-03-17 13:45:23.123456 UTC
```

## עיון מעמיק:
קבלת התאריך הנוכחי אינה חידוש ב-Haskell; זה עניין סטנדרטי בכל שפה. החבילה `time` ב-Haskell קיימת מזמן, והיא מאפשרת טיפול בתאריכים וזמנים עם דיוק גבוה. חלופות ל-`time` כוללות חבילות כמו `old-time` או גישות זמן מרובות בעידן פרה-`time`. פרטי היישום לפונקציה `getCurrentTime` כוללים קריאה למערכת ההפעלה כדי לקבל את הזמן לפי השעון האוניברסלי (UTC), והפונקציה עצמה מחזירה אובייקט מסוג `UTCTime`.

## ראה גם:
- המידע הרשמי על החבילת `time`: http://hackage.haskell.org/package/time
- מדריך כללי יותר לעבודה עם תאריכים וזמנים ב-Haskell: https://www.haskell.org/haskellwiki/Working_with_time
- דוקומנטציה לסוג `UTCTime`: http://hackage.haskell.org/package/time-1.9/docs/Data-Time-Clock.html#t:UTCTime

נקווה שעכשיו יש לך כלי יותר טוב לעבוד עם זמנים ותאריכים ב-Haskell. Happy coding!
