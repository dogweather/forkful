---
title:                "Haskell: קבלת התאריך הנוכחי."
simple_title:         "קבלת התאריך הנוכחי."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מדוע

כבר ניכנסנו לשנת 2021 וזהו זמן נהדר להתחיל ללמוד פרוגרמות בהסקל. אחת הדברים היסודיים שיש ללמוד בהסקל הוא כיצד לקבל את התאריך הנוכחי. תאריך זה יכול להיות שימושי למטרות מגוונות כמו ליצירת לוגים או לצפייה בנתונים בצורה ממושמת.

## כיצד לעשות זאת

בשפת הפרוגרמות הוסקל, קיימים מספר דרכים לקבלת התאריך הנוכחי. הדוגמאות הבאות מופיעות בקוד ```Haskell``` ומראות את תוצאות התאריך הנוכחי:

```Haskell
import Data.Time
getCurrentTime
-- 2021-06-15 16:45:22.3462693 UTC
```

```Haskell
import Data.Time
getCurrentDate
-- 2021-06-15
```

כפי שניתן לראות, נוכל לקבל את התאריך המלא או רק את התאריך בפורמט מסוים.

## צפייה עמוקה

כדי להבין על מה דברים מתבססים, ניתן להתבונן בקוד המקור של הפונקציות ```getCurrentTime``` ו-```getCurrentDate```. נשים לב ששתי הפונקציות מכילות בתוכן שימוש ב-```Data.Time``` שהוא חלק ממודול התאריך והזמן של הפרוגרמה. על מנת להשתמש בפונקציות אלה, ניתן ליצור גם עצמנו מודול תאריך וזמן או להתייחס ישירות למודול הקיים.

## ראה גם

- https://www.tutorialspoint.com/haskell/haskell_date_time.htm
- https://stackoverflow.com/questions/22992328/get-current-date-and-time-in-haskell
- https://www.haskell.org/hoogle/?hoogle=currentTime