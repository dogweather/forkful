---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Elm: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא אמור לקבוע תאריך מסוים מבלי לשנות את יום הזמן הנוכחי. זה מהותי לתכנתים כיוון שמטרדים זמניים נדרשים לאחזר את הנתונים היסטוריים או לתכנית את מהלכי הזמן הקרובים.

## הגדרה איך לעשות:
הראשון ציודים אישיים:
```Elm
import Time

type alias Date =
    { year : Int, month : Int, day : Int }

-- Create a date
someDate = Date 2022 07 04
```
עכשיו אנו מחשבים את התאריך 7 ימים בעתיד:
```Elm
Date (someDate.year) (someDate.month) (someDate.day + 7) 
-- Output: Date 2022 07 11
```
ו-30 ימים בעבר:
```Elm
Date (someDate.year) (someDate.month) (someDate.day - 30) 
-- Output: Date 2022 06 04
```
## צלילה עמוקה
בהקשר ההיסטורי, פונקציות מערכת הזמן הן חלק חיוני משנות ה-1970 כאשר שימוש במחשבים החל להיות נפוץ. ישנם אלטרנטיבות לביצוע חישובים של תאריכים, אבל הקוד של Elm הוא הרבה יותר קריא ופשוט. בנוסף, Elm מספק גם תמיכה מובנית לטיפוסים נוספים של נתוני זמן.

## ראה גם
דף הבית של Elm: [https://elm-lang.org](https://elm-lang.org)

מדריכים ל Elm : [https://elmprogramming.com](https://elmprogramming.com)

ספר דיגיטלי למשתמשים מתחילים של Elm : ["Elm in Action"](https://www.manning.com/books/elm-in-action)