---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
CSV זה פורמט קובץ שבו נתונים מופרדים בפסיקים. תוכניתנים עובדים איתו כי הוא פשוט, נפוץ, ונוח לייבוא וייצוא של נתונים.

## איך לעשות:
```Clojure
;; הוספת תלות בספריית clojure.data.csv
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

;; קריאה מקובץ CSV
(with-open [reader (io/reader "data.csv")]
  (doall
    (csv/read-csv reader)))

;; כתיבה לקובץ CSV
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["שם" "מייל"] ["גל" "gal@example.com"]]))
```
פלט דוגמא לקריאה:
`(["שם" "מייל"] ["גל" "gal@example.com"])`

פלט דוגמא לכתיבה:
קובץ `output.csv` עם התוכן:
```
שם,מייל
גל,gal@example.com
```

## נבחרת
הפורמט CSV התפתח מדרישות פשוטות בשנות ה-70. ישנם פורמטים אחרים כמו JSON ו-XML. ב-Clojure, עבודה עם CSV מתבצעת לעיתים קרובות דרך ספריית clojure.data.csv שמחבילה פונקציות לקריאה ולכתיבה.

## ראה גם
- [מדריך לספריית `clojure.data.csv`](https://github.com/clojure/data.csv)
- [תיעוד הרשמי של `clojure.data.csv`](https://clojure.github.io/data.csv/)
- [מאמר על עבודה עם קבצי CSV ב-Clojure](https://www.braveclojure.com/core-async/)
