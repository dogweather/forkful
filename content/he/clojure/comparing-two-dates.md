---
title:                "להשוואת שתי תאריכים"
html_title:           "Clojure: להשוואת שתי תאריכים"
simple_title:         "להשוואת שתי תאריכים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

מה ולמה?
להשוואת תאריכים היא פעולה שבה משווים בין שני תאריכים כדי לבדוק אם הם זהים, מיוחדים או אם אחד מהם מגיע לפני השני. החשיבה הפשוטה מאחורי זה היא שתאריכים הם נתונים מספריים מסודרים וניתן להשתמש בפונקציות חשבון כדי לבדוק את הקשר ביניהם. ניתן להשתמש בהשוואת תאריכים כדי לדוגמא לבדוק אם משהו עבר או עדיין בתוקף, וזו הסיבה שמתכנתים משתמשים בה כל כך הרבה.

איך לבצע:
```Clojure
;; ייבוא של הספרייה java.time שמאפשרת השוואת תאריכים
(ns compare-dates.core
    (:require [java.time]))

;; ליצור שני תאריכים כדי להשוות
(def date1 (LocalDate/now))
(def date2 (LocalDate/of 2021 07 01))

;; להשוות בין שני התאריכים באמצעות פונקציות השוואה של java.time
(= date1 date2) ;; תחזיר false כי התאריכים שונים
(= date1 date1) ;; תחזיר true כי התאריכים זהים
(< date1 date2) ;; תחזיר false כי date1 מגיע לפני date2

;; להדפיס את הפלט של השוואת התאריכים תחת הביטוי לוגי
(println (= "Date 1 is equal to Date 2: " (= date1 date2)))

;; יציג: Date 1 is equal to Date 2: false
```

עיון מעומק:
השוואת תאריכים היא תהליך שנעשה מאז זמן העתיק. לפני אנו משתמשים בפונקציות מוכנות לשוואת תאריכים, פתרונות מאותם ימים היו כוללים שימוש באבן קשתית או בקישוריות של התקופות השונות. בימים אלה, לשמחתנו, יש לנו כלים מתקדמים ויעילים כדי להשוות תאריכים.

אלטרנטיבות:
קיימות כמה אפשרויות אחרות לביצוע השוואת תאריכים. ניתן להשתמש בחבילות נוספות כמו clj-time או date-clj. אפשר גם להשתמש בפונקציות דטרמיניסטיות של התאריך העברי או הגרגוריאני כדי לבצע את ההשוואה.

פירוט טכני:
המימוש של השוואת תאריכים נעשה תוך שימוש במבנה הנתונים המעודכן של java.time. הפונקציות שמשמשות לשוואת תאריכים הן חכמות מאוד ומאפשרות למתכנתים לבצע שוואה בין כל סוגי התאריכים והקומפוננטות בדיוק. הן גם כוללות תמיכה במגוון רחב של תבניות תאריכים כגון ISO 8601.

ראו גם:
- [The Clojure Programming Language](https://clojure.org/)
- [Java.time Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clj-time Library](https://github.com/clj-time/clj-time)
- [Date-clj Library](https://github.com/sklower/date-clj)