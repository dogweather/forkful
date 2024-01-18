---
title:                "מהדורת תאריך ממחרזת."
html_title:           "Clojure: מהדורת תאריך ממחרזת."
simple_title:         "מהדורת תאריך ממחרזת."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

תיקון תאריך ממחרוזת הוא תהליך שבו מתאריך רווח תאריך ממחרוזת טקסטית. פעולה זו חשובה למתכנתים כי כאשר מנסים לעבוד עם נתונים ממין שונה, כמו תאריכים, נדרשים המרות כדי להפוך את הנתונים למבנים דטה שניתן לעבוד איתם.

## איך לעשות זאת?

Clojure מציע כמה אפשרויות לתיקון תאריך ממחרוזת. נבחר באחת מהן ונדגים כיצד להשתמש בה בעזרת קוד ותוצאות מדגמה.

```Clojure
(require '[clojure.java-time :as t])
```

שימוש בפונקציית צפייה (parse) כדי להמיר את המחרוזת לתאריך:

```Clojure
(t/local-date "30/11/2021")
=> #object[java.time.LocalDate 0x4597bcdf "2021-11-30"]
```

שימוש בטפסים של נתוני Java.time כדי ליצור אובייקט תאריך ישירות:

```Clojure
(import 'java.time.format.DateTimeFormatter)
(import 'java.time.LocalDate)

(def date-formatter (DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(LocalDate/parse "30/11/2021" date-formatter)
=> #object[java.time.LocalDate 0x1d26e08d "2021-11-30"]
```

## מיומנות עמוקות

תיקון תאריך ממחרוזת היה תהליך מורכב בשבעיםים, כאשר תכנות המחשב נמצא בשלבי התחילתיים. רק בעזרת טכנולוגיות חדשות יותר, כמו Java.time, למתכנתים יש כעת כלים מתקדמים יותר כדי לעבוד עם תאריכים ושעות. פונקציות נוחות כמו parse מוכנות בClojure כדי לפשט את התהליך של תיקון תאריכים ממחרוזת.

## ראו גם

למידע נוסף על איך להשתמש בתאריכים ושעות בClojure, אנא בקרו במדריך הרשמי של Clojure עבור Java.time: https://clojure.org/guides/java_interop