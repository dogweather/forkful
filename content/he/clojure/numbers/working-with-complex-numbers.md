---
title:                "עבודה עם מספרים מרוכבים"
aliases:
- /he/clojure/working-with-complex-numbers/
date:                  2024-01-26T04:39:52.080315-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-complex-numbers.md"
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
