---
date: 2024-01-20 17:32:40.272017-07:00
description: "\u05D1\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D3\u05E7\
  \u05D9\u05DD \u05D0\u05D9\u05D6\u05D4 \u05DE\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05E7\u05D3\u05DD \u05DC\u05E9\u05E0\u05D9 \u05D0\u05D5 \u05D4\u05D0\u05DD\
  \ \u05D4\u05DD \u05E9\u05D5\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05DE\u05D9\u05D9\u05DF \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\u05E0\
  \u05D4\u05DC \u05E1\u05E9\u05E0\u05D9\u05DD, \u05DC\u05EA\u05D6\u05DE\u05DF \u05E4\
  \u05E2\u05D5\u05DC\u05D5\u05EA \u05D5\u05E2\u05D5\u05D3."
lastmod: '2024-02-25T18:49:37.037683-07:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D0\u05E0\u05D5 \u05D1\u05D5\u05D3\u05E7\u05D9\
  \u05DD \u05D0\u05D9\u05D6\u05D4 \u05DE\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05E7\u05D3\u05DD \u05DC\u05E9\u05E0\u05D9 \u05D0\u05D5 \u05D4\u05D0\u05DD \u05D4\
  \u05DD \u05E9\u05D5\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05DE\
  \u05D9\u05D9\u05DF \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9\u05DD, \u05DC\u05E0\u05D4\
  \u05DC \u05E1\u05E9\u05E0\u05D9\u05DD, \u05DC\u05EA\u05D6\u05DE\u05DF \u05E4\u05E2\
  \u05D5\u05DC\u05D5\u05EA \u05D5\u05E2\u05D5\u05D3."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
בהשוואת שתי תאריכים אנו בודקים איזה מתאריכים קדם לשני או האם הם שווים. מתכנתים עושים זאת כדי למיין אירועים, לנהל סשנים, לתזמן פעולות ועוד.

## How to: (איך לעשות:)
```Clojure
;; הוספת הספרייה
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

;; יצירת שני אובייקטים של תאריכים
(def date1 (t/date-time 2023 3 25))
(def date2 (t/date-time 2024 3 25))

;; בדיקה מי מהם קדם
(t/before? date1 date2) ; => true
(t/after? date1 date2) ; => false

;; השוואה האם שני התאריכים שווים
(t/equal? date1 date2) ; => false

;; המרה מתאריך לטיימסטמפ והשוואה
(def timestamp1 (c/to-long date1))
(def timestamp2 (c/to-long date2))

(< timestamp1 timestamp2) ; => true
(= timestamp1 timestamp2) ; => false
```

## Deep Dive (עומק הנושא)
במקור השוואת תאריכים הייתה בעייתית בעיקר בגלל פורמטים שונים ואזורי זמן. Clojure, עם הספרייה `clj-time`, מספקת דרך מתוחכמת לטפל בתאריכים, ע"י עטיפה של מחלקת `java.util.Date`. ישנן גם אלטרנטיבות כמו `java.time` (Joda-Time) ל-Java 8 ולעיל, אבל `clj-time` נחשבת לבחירה פופולרית בקהילת Clojure.

למעשה, `clj-time` מתמקדת בפשטות ונוחות, תוך סיפקה של פונקציות חזקות לעבודה עם תאריכים וזמנים. עבור פרויקטים חדשים, מומלץ לשקול את `java.time`, שכן היא מובנית ב-Java ולכן לא דורשת ספריות חיצוניות.

## See Also (ראה גם)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [ClojureDocs - A community-powered documentation and examples repository for Clojure](https://clojuredocs.org/)
- [The Java Tutorials - Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
