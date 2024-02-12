---
title:                "המרת תאריך למחרוזת"
aliases:
- /he/clojure/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:43.613577-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

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
