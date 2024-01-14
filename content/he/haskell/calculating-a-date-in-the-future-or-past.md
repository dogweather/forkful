---
title:                "Haskell: חישוב תאריך בעתיד או בעבר"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# מדוע
מחשבות בגודלים כאשר מדובר בחישוב תאריך בעתיד או בעבר? כל שעליך לעשות הוא להחשיב כמה ימים, שבועות או אפילו שנים יש להוסיף או להוריד מהתאריך הנוכחי וזהו! החישוב של תאריך בעתיד או בעבר עלול להיות שימושי לשימושים שונים בתכנות בחירות פלטפורמות.

# כיצד לבצע
אם תרצה לחשב תאריך בעתיד או בעבר בשפת התכנות Haskell, הנה כמה דוגמאות של קוד ופלט נתונים תוך שימוש בתוכנית לחישוב תאריך:

``` Haskell
import Data.Time

-- חישוב התאריך הקרוב ביותר בעתיד
main = do
    today <- getCurrentTime
    let futureDate = addDays 7 today -- הוספת שבוע לתאריך הנוכחי
    print futureDate -- תאריך הקרוב בעתיד
```

אם רוצים לחשב תאריך בעבר, ניתן לממש את הפעולה הפונקצייתית `diffDays` מול תאריך הנוכחי כדי להוריד ימים מהתאריך. למשל:

``` Haskell
import Data.Time

-- חישוב התאריך הקרוב ביותר בעבר
main = do
    today <- getCurrentTime
    let pastDate = addDays (-30) today -- הוצאת 30 ימים מהתאריך הנוכחי
    print pastDate -- תאריך הקרוב בעבר
```

# חקירה עמוקה
חישוב תאריך בעתיד או בעבר לא מורכב בכלל וניתן לממשו בכמה שורות קוד פשוטות באמצעות הפונקציות הפונקציות `addDays` ו-`diffDays`. אם ברצונך ללמוד עוד על פעולות שיחזור מופעים חזרה ויצירת תאריכים נוספים, מומלץ לבדוק את החבילות המסופקות על ידי `Data.Time` בעת תכנות ב־Haskell.

# ראה גם
- [תעודת המדריך של Data.Time](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [חבילת Data.Time במדריך תכנותי בעברית](https://www.haskell.org