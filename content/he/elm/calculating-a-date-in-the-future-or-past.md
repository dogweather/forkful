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

# למה

חישוב תאריך בעתיד או בעבר יכול להיות שימושי כאשר צריך לתאר אירועים או תאריכים במסגרת שלושת השנים הבאות. זה יכול לעזור לתכנון תזמונים, חיבור תאריך התחלה ותאריך סיום, וכדי לקבוע תאריכים לאירועים מסוימים כגון חגים או ימי הולדת.

# איך לעשות זאת

לפניכם תוכן שלושת הדרכים הראשונים לחישוב תאריך בעתיד או בעבר בשפת Elm. כל דרך תכלול קוד דוגמה ותצוגת תוצאות כדי להבין את הפעולות הנדרשות.

```Elm
-- דרך 1: חישוב תאריך עתידי מתוך תאריך קיים
import Date

Date.now
    |> Date.add Date.Month 3
    |> Date.subtract Date.Day 5
    |> Date.toString
-- תוצאה: "2021-10-23"
```

```Elm
-- דרך 2: חישוב תאריך עתידי מתוך טווח תאריכים נתון
import Date

Date.fromCalendarDate 2021 12 31
    |> Date.add Date.Year 5
    |> Date.toString
-- תוצאה: "2026-12-31"
```

```Elm
-- דרך 3: חישוב תאריך בעבר מתוך טווח תאריכים נתון
import Date

Date.fromCalendarDate 2000 1 1
    |> Date.subtract Date.Year 10
    |> Date.toString
-- תוצאה: "1990-01-01"
```

# טביעה עמוקה

חישוב תאריך בעתיד או בעבר הוא פעולה נפוצה בתכנות עבור לוח השנה. בשפת Elm ישנן כמה פונקציות יעילות כדי לעזור במשימה הזאת, כך שניתן להשתמש בהן בקלות לכל מטרה שתגיע בדרך.

 # ראו גם

- [פונקציות פנמונית בְּכָל הימור](https://guide.elm-lang.org/error_handling/)

- [מסמכים רשמיים של שפת Elm](https://elm-lang.org/docs)