---
title:                "קבלת התאריך הנוכחי (Kablat hata'arikh hanokhi)"
html_title:           "Elm: קבלת התאריך הנוכחי (Kablat hata'arikh hanokhi)"
simple_title:         "קבלת התאריך הנוכחי (Kablat hata'arikh hanokhi)"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

התאריך הנוכחי הוא פעולה חשובה ונפוצה בתכנות. כמו שמשמעותו, הוא מחזיר את התאריך הנוכחי במחשב. תכנתנים עושים זאת לפעמים כדי לבדוק את התאימות של התאריך למטרות מסוימות או ליישומים שונים.

# איך לעשות:

```Elm
import Time exposing (..)

currentDate : Time.Posix
currentDate =
    Time.now
```
Output:
```Elm
16050969456031‬
```

# הצצה עמוקה:

## קישורים שימושיים:

* [רשימת פונקציות התאריך ב- Elm](https://package.elm-lang.org/packages/elm/time/latest/Time)
* [הסבר נרחב יותר על תאריך נוכחי ב-‫Coderslang‬](https://www.coderslang.com/blog/how-to-get-current-date-in-elm)
* [ראוויו משתמשים למנגנון תאריך ב- Elm](https://discourse.elm-lang.org/t/interview-with-author-of-elm-time-module/3598)

## אלטרנטיבות:

ישנן כמה אלטרנטיבות לשימוש במנגנון התאריך של Elm. בנוסף לקוד המוצג בסעיף "איך לעשות", ניתן להשתמש גם בתכונת time המתאימה להגדרת התאריך הנוכחי כתכונה במודל.

# ראה גם:

להבנת נושא התאריך הנוכחי ב- Elm, מומלץ לקרוא גם את הפוסט הנ"ל באתר Coderslang ולעיין בתיעוד הפונקציה Time.now הקשורה לנושא זה. בנוסף, ניתן להתנסות במנגנון זה דרך תרגילי תירוצים ב- Codewars או להשתתף בפורום המשתמשים שלט אלם כדי לשאול שאלות נוספות בנושא זה.