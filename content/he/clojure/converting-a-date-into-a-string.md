---
title:                "Clojure: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה:

למה כדאי ללמוד כיצד להמיר תאריך למחרוזת בקלות וביעילות היישמות? כי השימוש בתאריך וזמן הינו חלק חשוב ממנגזרתי התוכנה, ולמידתו תעזור לנו ליצור קוד יתר הדימיון ונקי.

## כיצד לעשות:

באמצעות השימוש בשפת Clojure, נוכל בקלות להמיר תאריך למחרוזת על ידי שימוש בפונקציות פנימיות בשפה זו. למטה תוכלו למצוא כמה דוגמאות כיצד לעשות זאת בקוד והפלט המתקבל מכל דוגמה.

```Clojure
;; ייבוא הספריה לטיפול בתאריכים
(require '[clj-time.core :as t])
;; המרת תאריך למחרוזת
(t/format (t/now) "dd/MM/yyyy")
;; הפלט: "28/06/2021"

;; המרת תאריך למחרוזת + הוספת זמן
(t/format (t/plus (t/now) (t/day 10)) "dd/MM/yyyy HH:mm")
;; הפלט: "08/07/2021 16:05"

;; המרת תאריך למחרוזת + הגדלת החודש
(t/format (t/plus-months (t/now) 3) "MMMM yyyy")
;; הפלט: "September 2021"
```

## Deep Dive:

כדי להמיר תאריך למחרוזת באופן מדויק, ניתן להשתמש בפונקציות הפנימיות הנגישות בשפת Clojure. כל תאריך מיוצג כ-"משחק של דמיון" ולכן אנו יכולים לתמצת את הפונקציות הדרושות להמרתו למחרוזת בצורה יעילה ונקייה. בנוסף, ישנן ספריות נוספות ותוספים לשפה שיכולים לסייע בהמרת תאריך למחרוזת בדרכים שונות, כך שיש לנו מגוון אפשרויות לבחירה.

## ראה גם:

- https://clojuredocs.org/clojure.java-time/format
- https://github.com/clj-time/clj-time
- https://github.com/gchp/Pretty-Time-Clj