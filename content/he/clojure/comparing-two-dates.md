---
title:                "Clojure: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מדוע

השוואת שתי תאריכים היא מתוך רצונך להשוות אירועים, לימים מפורטים או לבדיקת גיל. ישנם מספר סיבות למה זה יכול להיות שימושי בתכנות בקלות, והכי חשוב - מבלי להתעטף בקורות גלישה בגיאוגרפיות.

## איך

כאשר מדברים על השוואת תאריכים בקלות בשפת Clojure, ישנם מספר אפשרויות שיכולים לעזור לך להשוות שתי תאריכים בצורה ברורה ויעילה. הנה כמה דוגמאות לקוד ופלט המדגימים את השימוש במספר פונקציות של Clojure עבור השוואת תאריכים:

```Clojure
;; התאריך הנוכחי
(def date1 (java.time.LocalDate/now))

;; תאריך עתידי ב-3 חודשים
(def date2 (java.time.LocalDate/now).plusMonths(3))

;; השוואה בין התאריכים לפי סדר כרונולוגי
(println (compare date1 date2)) ; 1

;; השוואה בין התאריכים לפי אזור הזמן שלהם
(println (compare date1 date2 java.time.zone.ZoneId/systemDefault)) ; 1

;; הדפסת המרחק בין שני התאריכים בימים
(println (Math/abs (.getDays (java.time.temporal.ChronoUnit/DAYS.between date1 date2)))) ; 90

```

## צלילת עמוקה

בשפת Clojure ישנן מספר פונקציות מובנות שיכולות לעזור לך לבצע השוואה בין תאריכים בצורה מדויקת ומדוייקת יותר. פונקציות כמו `compare` ו- `between` מקלות על משימה זו ומאפשרות לך להתאים את האלגוריתם לצרכים המדויקים שלך.

## ראה גם

- [מדריך לפונקציות זמן בשפת Clojure](https://clojuredocs.org/clojure.java-time)
- [השוואת שני תאריכים במדריך זמן ב-Python](https://realpython.com/comparing-python-dates/)
- [מידע נוסף על פונקציות זמן בשפת Clojure](https://www.clojure.org/guides/dates)