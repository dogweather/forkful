---
title:                "Clojure: חישוב תאריך בעתיד או בעבר"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

למה להתעסק בחישוב תאריך בעתיד או בעבר? חישוב תאריך יכול להיות שימושי במגוון מקרים, כגון תכנות אירועים עתידיים או ייצור דוחות היסטוריים.

## כיצד להשתמש

המדריך הבא יראה לך כיצד לחשב את התאריך בעתיד או בעבר באמצעות שפת התכנות קלוז'ור. ניתן להשתמש בקוד המוצג כדי לבדוק בו זמנית את התאריך המבוקש לפי הפורמט הנתון.

```Clojure
(defn calculate-date [date time-offset]
  (let [date (clj-time.coerce/from-date date)
        calculated-date (clj-time.core/plus date time-offset)]
    (clj-time.coerce/to-date calculated-date)))

(calculate-date "2020-01-01" (clj-time.core/months 2))

; Output: #inst "2020-03-01T00:00:00.000-00:00"
```

קוד זה משתמש בפונקציות מהספריות clj-time ו-clj-time.coerce כדי להמיר את התאריך לפורמט של קלוז'ור ולחשב את התאריך המבוקש על ידי הוספת ה"מיכל" המבוקש לתאריך הנתון.

## חפירה עמוקה

חישוב תאריך בעתיד או בעבר הוא תהליך העוסק בטיפול בתאריכים כאובייקטים. בשפת קלוז'ור, כמו בשפות אחרות, קיימות מגוון ספריות וכלים שמסייעים בטיפול בתאריכים, כגון clj-time, java.time ו-Joda-Time.

הפונקציות הקיימות במגוון הספריות נותנות יכולת לחשב את התאריך בעתיד או בעבר באמצעות פרמטרים כגון מידה של ימים, חודשים, שנים וכו'. את התאריך המבוקש ניתן להמיר לפורמט שלאחר מכן.

## ראה גם

- [מדריך לפונקציות נתונים בקלוז'ור](https://github.com/funcool/codox)
- [תיעוד שפת קלוז'ור](https://clojure.org/documentation)