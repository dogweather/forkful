---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרה של תאריך למחרוזת מאפשרת לנו להציג את התאריך בצורה 'אנושית', כגון "23 בנובמבר, 2021". זה מאוד נוח לעיבוד והצגה של נתונים, לתקשורת בין יישומים, הדפסה ואחסון.

## איך לעשות:

להפוך תאריך למחרוזת בClojure הוא תהליך ישר. Clojure משתמשת בJava Interop ליישום זה:

```clojure
(import 'java.text.SimpleDateFormat)

(defn date-to-string [date format]
  (.format (SimpleDateFormat. format) date))

(def sample-date (java.util.Date.))
(println (date-to-string sample-date "dd MMMM, yyyy"))
```

זה יחזיר מחרוזת כגון "23 בנובמבר, 2021".

## צלילה עמוקה:

בעבר, בחלק מהשפות נעשתה הפיכה של תאריך למחרוזת באופן ידני, שבו נדרש לטפל במגבלות השמות המשתנים של החודשים והקפיצופים. היום, שפות רבות כמו Clojure משתמשות בממשקי Java (או ספריות חיצוניות) לביצוע המרה זו.

אלטרנטיבות אחרות כוללות שימוש בספריות אחרות, כמו clj-time, שמאפשרת שליטה גדולה יותר, תמיכה באזורי זמן, פורמטים מותאמים אישית ועוד.

המרה של תאריך למחרוזת היא בנית נמוכה של מסגרת מידע Java, Java Interop, מה שמאפשר לנו לקבל משאבים רבים וחזקים של Java.

## ראה גם:

[Joda-Time](http://www.joda.org/joda-time/)
[clj-time library](https://github.com/clj-time/clj-time)
[Java SimpleDateFormat documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)