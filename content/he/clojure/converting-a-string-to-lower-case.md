---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:17.905207-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פיכת מחרוזת לאותיות קטנות זו פעולה שמשנה את כל התווים במחרוזת לגרסתם הקטנה. תוכניתנים עושים זאת כדי לאחד פורמטים, להשוות מחרוזות בלי להתחשב ברישיות, או לפרוסס טקסט.

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