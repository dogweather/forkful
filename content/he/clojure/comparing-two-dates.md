---
title:                "Clojure: השוואת שתי תאריכים"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה
שפת התכנות Clojure מציעה דרך יעילה ופשוטה להשוות שתי תאריכים. כתיבת קוד להשוואת תאריכים עשויה להיות טריקייה ומורכבת בשפות תכנות אחרות, אך Clojure מציגה פתרון יעיל וקריא לבעיה זו.

## איך לעשות זאת
```Clojure
(require '[clojure.java-time :as t])

(def date1 (t/local-date 2021 4 1))
(def date2 (t/local-date 2021 8 15))

(t/before? date1 date2) ; => true
(t/after? date1 date2) ; => false
(t/minus date2 (t/period 3 :months)) ; => #object[java.time.LocalDate 0x25bfb09c "2021-05-15"]
(t/equals? date1 date2) ; => false
(t/days-between date1 date2) ; => 136
```

קוד זה מדגים את השימוש בספריית clojure.java-time כדי להשוות שתי תאריכים, להחיל מגבלות על תחום התאריכים ולמצוא את הימים הממוצעים ביניהם. הנה קצת משתנה למיתוג הם לחץ לעזור לך להתרגל לקוד Clojure:

- ````Clojure
  (t/period 3 :months) ; => #object[java.time.Period 0x138c7da5 "P3M"]
  ````
- ````Clojure
  (t/plus date1 (t/new-duration 8 :days)) ; => #object[java.time.LocalDate 0x1f5e7b19 "2021-04-09"]
  ````

## חפירה עמוקה
כמו כל פעולה בשפת Clojure, גם השוואת תאריכים מבוססת על פעולות על צמתי הנתונים. תחילה, ניתן ליצור תאריכים מסוג LocalDate באמצעות הפונקציה t/local-date ולבצע פעולות יישומיות עליהם עם הפונקציות המקומיות של ספריית java.time. ניתן גם להשתמש בספריית clojure.java-time כדי לגשת לפונקציות נוספות כמו השוואה, יצירת תאריך חדש עם הוספת זמן נוסף או צמתי הנתונים כדי למצוא את הימים בין התאריכים.

## ראה גם
- [ספריית clojure.java-time](https://clojure.github.io/java-time/)
- [מדריך לפתרונות של תאריכים