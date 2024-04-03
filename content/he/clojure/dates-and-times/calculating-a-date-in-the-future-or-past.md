---
date: 2024-01-20 17:31:03.144863-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\
  \u05E2\u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05D4\u05D5\u05E1\u05E4\u05D4 \u05D0\
  \u05D5 \u05D4\u05E4\u05D7\u05EA\u05D4 \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD, \u05E9\
  \u05D1\u05D5\u05E2\u05D5\u05EA, \u05D7\u05D5\u05D3\u05E9\u05D9\u05DD \u05D0\u05D5\
  \ \u05E9\u05E0\u05D9\u05DD \u05DE\u05EA\u05D0\u05E8\u05D9\u05DA \u05E0\u05EA\u05D5\
  \u05DF. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05DC\u05E6\u05E8\u05DB\u05D9 \u05EA\u05DB\u05E0\u05D5\u05DF\
  , \u05D2\u05D1\u05D9\u05D9\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.726678-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8 \u05D4\u05D5\u05D0 \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05D4\u05D5\u05E1\u05E4\u05D4 \u05D0\u05D5\
  \ \u05D4\u05E4\u05D7\u05EA\u05D4 \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD, \u05E9\u05D1\
  \u05D5\u05E2\u05D5\u05EA, \u05D7\u05D5\u05D3\u05E9\u05D9\u05DD \u05D0\u05D5 \u05E9\
  \u05E0\u05D9\u05DD \u05DE\u05EA\u05D0\u05E8\u05D9\u05DA \u05E0\u05EA\u05D5\u05DF\
  ."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

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
