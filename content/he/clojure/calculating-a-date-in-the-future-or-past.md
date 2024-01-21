---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:03.144863-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה של הוספה או הפחתה של ימים, שבועות, חודשים או שנים מתאריך נתון. תכנתים מבצעים את זה לעיתים קרובות לצרכי תכנון, גביית נתונים או פיצ'רים הקשורים בזמן.

## איך לעשות:
```Clojure
;; נשתמש בספריית clj-time
(require '[clj-time.core :as t]
         '[clj-time.format :as fmt]
         '[clj-time.periodic :as p])

;; יצירת תאריך
(def my-date (t/now))

;; להוסיף 3 ימים לתאריך
(def future-date (t/plus my-date (t/days 3)))

;; להפחית 5 שנים מתאריך
(def past-date (t/minus my-date (t/years 5)))

;; דוגמה לפלט
(println (fmt/unparse (fmt/formatters :basic-date-time) future-date))
;; => "20230217T103000.000Z"

(println (fmt/unparse (fmt/formatters :basic-date-time) past-date))
;; => "20180217T103000.000Z"
```

## עיון נוסף
הספריה `clj-time` מבוססת על Joda-Time, ספרייה פופולרית של ג'אווה לניהול זמנים ותאריכים. חלופות כוללות את ספריית java.time של Java 8 והלאה. בחירת הספריה תלויה בדרישות הפרויקט ובהעדפות האישיות. כשעובדים עם תאריכים, חשוב להבין את המושגים של אזורי זמן והתאמות עונתיות כדי להימנע מבאגים. 

## ראו גם
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [Joda-Time documentation](https://www.joda.org/joda-time/)
- [java.time package summary](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)