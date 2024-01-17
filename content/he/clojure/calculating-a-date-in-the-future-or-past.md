---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Clojure: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?

חישוב תאריך בעתיד או בעבר הוא פעולה שמשמשת מתכנתים לחישוב תאריכים על פי תנאים נתונים. זה יכול להיות מועיל לצורך יצירת קודים מחשבוניים, תכניות תרחיש ומטריצות עבור התאריכים.

## איך לעשות זאת?

לחישוב תאריך בעתיד או בעבר ב-Clojure ניתן להשתמש בפונקציות המובנות `clj-time` ו `calendrical`.

```Clojure
(require '[clj-time.core :as time])

; לחישוב תאריך בעתיד:
(time/plus (time/now) (time/days 10))

; לחישוב תאריך בעבר:
(time/plus (time/now) (time/weeks -2))

; פלט:
#<DateTime 2020-04-22T21:41:47.061Z>
```

## חקירה מעמיקה

### היסטוריה

חישוב תאריך בעתיד או בעבר הוא תוכנית ארכיון ישנה שנמצאת בשימוש כבר מזמן רב. כיום, ישנם מספר פתרונות אחרים לחישוב תאריך בעתיד או בעבר כגון ספריות ומודולים אחרים המציעים פונקציונליות דומה.

### אלטרנטיבות

בנוסף לפתרונות המובנים של Clojure, ישנם גם ספריות חיצוניות שנועדו לעזור ללמוד ולחשב תאריכים בעתיד או בעבר. כמה מהן כוללות את `java.time` ו- `org.joda.time`.

### פרטי היישום

כאשר משתמשים בפונקציות המובנות של Clojure לחישוב תאריך בעתיד או בעבר, יש לקחת בחשבון את הפורמטים השונים של תאריכים בפלט ולהתאים אותם לצרכי האפליקציה שלכם.

## ראו גם

- [clj-time Dokumentacio](https://clj-time.github.io/clj-time/doc/index.html)
- [calendrical Dokumentacio](https://github.com/clj-commons/calendrical)