---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-stderr היא שליחת הודעות שגיאה או אזהרות לפלט סטנדרטי מיוחד שאינו הזרם הרגיל לפלט. פרוגרמים עושים זאת כדי להבדיל בין הודעות שגיאה לבין פלט רגיל של התוכנית, מה שמאפשר ניהול וניטור יעיל של בעיות.

## איך לעשות:
```Clojure
;; שליחת מחרוזת ל-stderr
(.write *err* "שגיאה: קרתה בעיה\n")

;; דוגמא לשליחת הודעת שגיאה עם פרטי חריגה
(try
  (throw (Exception. "משהו השתבש"))
  (catch Exception e
    (.write *err* (str "שגיאה: " (.getMessage e) "\n"))))
```
תוצאה:
```
שגיאה: קרתה בעיה
שגיאה: משהו השתבש
```

## הצלילה לעומק
stderr היא מושג שמקורו בסביבות עבודה יוניקס ולינוקס, שימשה כאמצעי להפריד שגיאות מהודעות רגילות. ב-Clojure, אנחנו יכולים לכתוב ל-stderr דרך הגלובל `*err*`. חלופות כוללות כתיבה לקובץ לוג, שליחת הודעות ל-service ניטור חיצוני, או קולטים לוגים כמו log4j. ברמת המימוש, כתיבה ל-stderr אינה חלק מהפלט הרגיל של התוכנית ויכולה להינתב באופן נפרד בסביבת הביצוע, לדוגמא על ידי הפניית זרמים של מערכת ההפעלה.

## ראו גם:
- [ClojureDocs on *out* and *err*](https://clojuredocs.org/clojure.core/*err*)
- [Using Java's System.err in Clojure](https://clojure.org/reference/java_interop#_system_out_and_system_err)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
