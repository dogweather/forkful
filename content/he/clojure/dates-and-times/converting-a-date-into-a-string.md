---
date: 2024-01-20 17:37:43.613577-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\u05D8. \u05D0\u05E0\u05D5\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\
  \u05E6\u05D9\u05D2 \u05D0\u05D5\u05EA\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\
  \u05D3\u05D9\u05D3\u05D5\u05EA\u05D9 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\
  \u05D5 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05D5\u05EA\u05DD \u05DC\u05EA\u05E7\
  \u05E0\u05D9\u05D9\u05DD \u05DC\u05E9\u05D9\u05EA\u05D5\u05E3 \u05D5\u05D0\u05D7\
  \u05E1\u05D5\u05DF."
lastmod: '2024-03-13T22:44:38.723573-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E9\
  \u05DC \u05E9\u05D9\u05E0\u05D5\u05D9 \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\u05D8. \u05D0\u05E0\u05D5 \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E6\
  \u05D9\u05D2 \u05D0\u05D5\u05EA\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D9\u05D3\
  \u05D9\u05D3\u05D5\u05EA\u05D9 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5\
  \ \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05D5\u05EA\u05DD \u05DC\u05EA\u05E7\u05E0\
  \u05D9\u05D9\u05DD \u05DC\u05E9\u05D9\u05EA\u05D5\u05E3 \u05D5\u05D0\u05D7\u05E1\
  \u05D5\u05DF."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## What & Why? (מה ולמה?)
המרת תאריך למחרוזת היא משימה של שינוי פורמט התאריך לטקסט. אנו עושים זאת כדי להציג אותם באופן ידידותי למשתמש או להפוך אותם לתקניים לשיתוף ואחסון.

## How to: (איך לעשות:)
```clojure
;; ייבוא החבילה java.text שמאפשרת עיבוד תאריכים
(require '[java.text :as fmt])

;; יצירת פורמט תאריך לדוגמא
(def my-date-formatter (fmt/SimpleDateFormat. "dd-MM-yyyy HH:mm:ss"))

;; המרת אובייקט Date למחרוזת
(defn date-to-string [date]
  (.format my-date-formatter date))

;; שימוש בפונקציה עם תאריך נוכחי
(println (date-to-string (java.util.Date.)))
```

פלט לדוגמה:
```
"25-03-2023 14:50:31"
```

## Deep Dive (צלילה לעומק):
במאה ה-21, פעולות על תאריכים הם לחם יומי בתכנות. כדי להבין מדוע זה קריטי, חשוב להכיר את היסטוריה של שימוש בתאריכים במחשבים. התחלנו עם פורמטים פשוטים, אבל עם הזמן הופעלה הדרישה לטיפול מסודר יותר ותאריכים בעלי אזורי זמן ותרבויות שונות.

ב-Clojure, מומלץ להשתמש ב-Java interop כיילו היא שפה שנבנית על JVM (Java Virtual Machine). יש חבילות Java רבות וקיימות לעיבוד תאריכים, כמו `java.text.SimpleDateFormat`, שמשמשת בדוגמה שלמעלה.

אפשר להשתמש גם בחבילות נוספות כמו `clj-time` שמציעה ממשק קלוז'רי נעים יותר, אבל עבור פרויקט פשוט או רכיב מסוים, הספרייה הסטנדרטית של Java עלולה להיות מספיקה.

## See Also (ראה גם):
- [JavaDoc for SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html) - מסמך ה-JavaDoc של `SimpleDateFormat` להכרת כל אפשרויות הפורמט.
- [clj-time GitHub repository](https://github.com/clj-time/clj-time) - GitHub של `clj-time`, ספרייה לטיפול בתאריכים בקלוז'ר.
- [ClojureDocs](https://clojuredocs.org/) - מדריך קלוז'ר עם דוגמאות ותיאורים מהקהילה.
