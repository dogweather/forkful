---
title:    "Elm: חישוב תאריך בעתיד או בעבר"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

אלמנטי האלם הם כלי חזקים שנועדו לסייע לכם ליצור אפליקציות פתוחות קוד חדשניות ואטרקטיביות. אחת התכונות המעניינות ביותר של אלמנטם היא היכולת לחשב תאריכים בעתיד או בעבר. זה נחוץ כאשר אתם עובדים עם אפליקציות כמו לוח שנה או תזכורות, וגם עבור דרישות של פרויקטים מתנדבים כגון עמותות או מגמות.

## איך לעשות זאת

אם אתם מעוניינים לחשב תאריכים בעתיד או בעבר עם אלם, תישארו לשים! נדגים כמה דוגמאות קוד וכיצד תיצור את התוצאה הרצויה.

\`\`\`Elm
import Time exposing (..)
import Time.Date exposing (..)

-- חישוב תאריך בעבור 3 ימים מהיום
futureDate : Date
futureDate = add (days 3) (round (posixToMillis (Time.now))) 

-- חישוב תאריך בעבור 2 שבועות ו5 ימים לפני היום
pastDate : Date
pastDate = add (days (-5)) (add (weeks (-2)) (round (posixToMillis (Time.now))))

-- חישוב תאריך לתאריך מסוים בחודש הזה
specificDate : Date
specificDate = Date.fromYearMonthDate 2021 9 20

-- הדפסת התאריכים
main : Html msg
main =
    div []
        [ text (toString futureDate)
        , text (toString pastDate)
        , text (toString specificDate)
        ]
        
-- תוצאות:
-- 2021-09-16
-- 2021-08-29
-- 2021-09-20
\`\`\`

## טיול עמוק

עכשיו שאתם מבינים איך לחשב תאריכים בעתיד או בעבר עם אלם, בואו נחקור קצת יותר עמוק. כתבתי כמה פונקציות במודול Time.Date שישמשו אתכם בכל מיני סיטואציות.

- add : מוסיף פונקציות זמן של ימים, שבועות, חודשים או שנים לתאריך קיים.
- fromYearMonthDate : יוצר תאריך חדש משנה, חודש ויום.
- DayOfWeek : מציג את היום בשבוע לפי נומר