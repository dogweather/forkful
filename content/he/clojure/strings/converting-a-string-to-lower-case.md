---
date: 2024-01-20 17:38:17.905207-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05D1\u05E7\
  \u05DC\u05D5\u05D6'\u05E8, \u05EA\u05D5\u05DB\u05DC\u05D5 \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 `clojure.string/lower-case`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05DE\u05D9\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA\
  . \u05D6\u05D4 \u05DB\u05DC \u05DE\u05D4 \u05E9\u05E6\u05E8\u05D9\u05DA."
lastmod: '2024-03-13T22:44:38.683246-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E7\u05DC\u05D5\u05D6'\u05E8, \u05EA\u05D5\u05DB\u05DC\u05D5 \u05DC\
  \u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ `clojure.string/lower-case` \u05DB\u05D3\u05D9 \u05DC\u05D4\u05DE\u05D9\u05E8\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA\
  \ \u05E7\u05D8\u05E0\u05D5\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

## How to: (איך לעשות:)
בקלוז'ר, תוכלו להשתמש בפונקציה `clojure.string/lower-case` כדי להמיר מחרוזת לאותיות קטנות. זה כל מה שצריך.

```clojure
(require '[clojure.string :as str])

;; המרת מחרוזת לאותיות קטנות
(str/lower-case "Hello, World!")
;; פלט: "hello, world!"
```

## Deep Dive (צלילה עמוקה)
הפונקציה `lower-case` במערכת הפונקציות של Clojure נחלקת לשתי קטגוריות: פונקציונליות עבור פיתוח יישומים והפונקציונליות הפנימית של JVM, שזו עליה Clojure בנויה. Clojure היא למעשה נושאת משאלת פנים של גוונים פונקציונליים, עם היסטוריה החלה ב-2007, ומעצבת את הדרך שבה אנו חושבים על מניפולציה של מחרוזות ונתונים.

גרסאות קודמות נזקקו לגישות יותר "ידניות" או חיצוניות כמו השימוש ב-Java interop (האינטראקציות בין Clojure ל-Java) לצורך המרת מחרוזות, אך היום המתודולוגיה שנבחרה פשוטה ונקייה.

```clojure
;; דוגמה נוספת עם Java interop
(.toLowerCase "Hello, World!")
;; פלט: "hello, world!"
```

הבחירה בין השיטות תלויה בהעדפות המתכנת ובהקשר שבו הוא עובד. השימוש בפונקציות ספציפיות של Clojure מועדף לרוב בשל התמיכה בפרדיגמה הפונקציונלית והקוליוקוויאלית של השפה.

## See Also (ראו גם)
- התיעוד הרשמי של Clojure לעבודה עם מחרוזות: https://clojure.org/guides/learn/strings
- פוסט בקהילה על מניפולציות של מחרוזות ב-Clojure: http://clojure-doc.org/articles/tutorials/strings.html
- אוסף שאילתות רלוונטיות ב-StackOverflow: https://stackoverflow.com/questions/tagged/clojure+string
