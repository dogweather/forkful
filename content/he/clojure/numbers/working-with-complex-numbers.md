---
date: 2024-01-26 04:39:52.080315-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05E8\u05D7\u05D9\u05D1\u05D9\u05DD \u05D0\u05EA \u05D4\u05DE\u05E1\
  \u05E4\u05E8\u05D9\u05DD \u05D4\u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05E2\u05DD\
  \ \u05D7\u05DC\u05E7 \u05E0\u05D5\u05E1\u05E3, \u05D9\u05D7\u05D9\u05D3\u05D4 \u05D3\
  \u05DE\u05D9\u05D5\u05E0\u05D9\u05EA 'i'. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05D1\u05EA\u05D7\u05D5\
  \u05DE\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD, \u05DB\u05D5\u05DC\u05DC \u05E2\
  \u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\u05D5\u05EA, \u05EA\u05D0\u05D5\u05E8\
  \u05D9\u05D4 \u05D0\u05DC\u05E7\u05D8\u05E8\u05D5\u05DE\u05D2\u05E0\u05D8\u05D9\u05EA\
  ,\u2026"
lastmod: '2024-03-13T22:44:38.694287-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\
  \u05DD \u05DE\u05E8\u05D7\u05D9\u05D1\u05D9\u05DD \u05D0\u05EA \u05D4\u05DE\u05E1\
  \u05E4\u05E8\u05D9\u05DD \u05D4\u05DE\u05DE\u05E9\u05D9\u05D9\u05DD \u05E2\u05DD\
  \ \u05D7\u05DC\u05E7 \u05E0\u05D5\u05E1\u05E3, \u05D9\u05D7\u05D9\u05D3\u05D4 \u05D3\
  \u05DE\u05D9\u05D5\u05E0\u05D9\u05EA 'i'. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05D1\u05EA\u05D7\u05D5\
  \u05DE\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD, \u05DB\u05D5\u05DC\u05DC \u05E2\
  \u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\u05D5\u05EA, \u05EA\u05D0\u05D5\u05E8\
  \u05D9\u05D4 \u05D0\u05DC\u05E7\u05D8\u05E8\u05D5\u05DE\u05D2\u05E0\u05D8\u05D9\u05EA\
  ,\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מורכבים מרחיבים את המספרים הממשיים עם חלק נוסף, יחידה דמיונית 'i'. תכנתים משתמשים בהם בתחומים שונים, כולל עיבוד אותות, תאוריה אלקטרומגנטית, ופרקטלים, שם חישובים הכרוכים בשורש ריבועי של מספר שלילי הם שגרתיים.

## איך ל:
Clojure מספקת תמיכה מובנית למספרים מורכבים דרך המחלקה השימושית `clojure.lang.Numbers`. השתמשו ב`complex` כדי ליצור מספרים מורכבים ולבצע חישובים אריתמטיים.

```clojure
;; יצירת מספרים מורכבים
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; חיבור
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; חיסור
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; כפל
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; חילוק
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; צמוד
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## עיון מעמיק
מספרים מורכבים הופורמלו על ידי מתמטיקאים כמו גאוס ואוילר במאה ה-18. למרות שבתחילה נתקלו בספקנות, מאז הם הפכו להכרחיים במדע והנדסה המודרניים. ל-Clojure אין סוג מספר מורכב טבעי כמו בחלק מהשפות (למשל, Python), אך ה-interop עם Java יכול לטפל בפעולות הדרושות דרך המחלקה `clojure.lang.Numbers`.

`java.lang.Complex` של Java הוא חלופה אמינה, המספקת יותר תכונות ואפשרויות לאופטימיזציה. אינטרופרביליות המארח של Clojure מקלה על העבודה עם ספריות Java.

מאחורי הקלעים, חשבון מספרים מורכבים כולל חיבור וכפל של החלקים הממשיים והדמיוניים, עם הכלל המרכזי ש-`i^2 = -1`. חילוק מספרים מורכבים יכול להיות יותר מסובך, ולרוב דורש את הצמוד כדי להימנע מחילוק במספרים מורכבים.

## ראו גם
- ה-ClojureDocs, לעיון מהיר: https://clojuredocs.org/
- ה-API של Java ל-`java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- דף הוויקיפדיה על מספרים מורכבים לסקרנים המתמטיים: https://en.wikipedia.org/wiki/Complex_number
