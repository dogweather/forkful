---
title:                "כתיבה לשגיאת תקן"
html_title:           "Clojure: כתיבה לשגיאת תקן"
simple_title:         "כתיבה לשגיאת תקן"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# מה ולמה?
כתיבה לפלט שגיאות סטנדרטי (standard error) היא כתיבה של הודעות שגיאה ואזהרות לסיוע למתכנתים בזמן הרצת קוד. מתכנתים משתמשים בכתיבה לפלט שגיאות סטנדרטי כדי לקבל חזרה מיידית על טעויות בקוד ולתקן אותם במהירות.

# איך לעשות?
כתיבת הודעות לפלט שגיאות סטנדרטי ב-Clojure נעשית באמצעות הפונקציה (ef unerror דרך (System.err.println "הודעת שגיאה"). הנה דוגמא לקוד והפלט של הודעת שגיאה סטנדרטית:

```Clojure
(defn divide [x y]
  (if (zero? y)
    (System.err.println "אפס אינו תקף עבור מחלק")
    (println (/ x y))))

(divide 10 0)
```

הפלט של הודעת שגיאה יהיה:

```Clojure
אפס אינו תקף עבור מחלק
```

# עיון מעמיק
כתיבה לפלט שגיאות סטנדרטי התחילה כקונספט בשפת תכנות C ומאז התפתחה לעוד שפות תכנות רבות כולל Clojure. פונקציות נוספות שמשמשות לכתיבה לפלט סטנדרטי הן (ef unanger ו-ef errtrace וכן System.err/println. אופציה אחרת לכתיבה לפלט סטנדרטי היא שימוש בתוכניות טיפוסים כמו Log4j.

# ראה גם
למידה נוספת על כתיבה לפלט שגיאות סטנדרטי בעזרת Clojure: https://clojuredocs.org/clojure.core/ef-unerror.