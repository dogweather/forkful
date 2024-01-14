---
title:    "Clojure: כתיבה לשגיאה סטנדרטית"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## מדוע

הכתיבה לפלט שגיאות תקף רק בהתחלה. לכן, כתיבה לפלט היא תהליך חשוב בתכנות של הקוד השלם כדי להבין על מה לשפר ולתקן.

## איך לכתוב לפלט

מתוך העשרות של שגיאות ותכנים חדשים כל יום, לא תמיד ניתן להבין מה יוצר את השגיאה. עם תכנות לפלט הפונקציות צריכות להיות בקרה מתוך קוד כדי לזהות בדיוק איזה חלק מהקוד יוצר את הבעיה.

```Clojure
(defn divide-by-zero [x y]
  (if (= y 0)
    (print "Numerator cannot be zero") ; הודעת שגיאה
    (/ x y)))
```

פלט:

```
Numerator cannot be zero
```

## חקירה מעמיקה

הודעות שגיאה מציגות מידע רב יותר מאשר רק את השגיאה עצמה. הן יכולות לכוון לחלק מן הקוד הגורם לבעיה על ידי מזהים את המדוע לשגיאה. כמו כן, ניתן גם לדעת על תנאים מיוחדים אשר גרמו לשגיאה לכדי הבעיה.

לדוגמה, אם אנחנו משתמשים במתודת `try...catch` כדי להתמודד עם שגיאות, ניתן להדפיס את השגיאה המקורית כך שנוכל לבחון את הנתונים הפנימיים שלה.

```Clojure
(try
 (/ x y)
  (catch Exception e (println "Original error:" (.getMessage e))))
```

פלט:

```
Original error: ArithmeticException Divide by zero (divide-by-zero 12 0)
```

## ראו גם

- הסבר נוסף על כתיבה לפלט ניתן למצוא במדריך הרשמי של קלז'ור [Writing to Standard Error](https://clojure.org/guides/debugging#error_handling).
- כיצד להתמודד עם שגיאות בקוד קלז'ור במאמר [Error Handling in Clojure](https://www.baeldung.com/clojure-error-handling).
- הסבר על שימוש ב `try...catch` בשפת קלז'ור ניתן למצוא במדריך הרשמי של קלז'ור [Handling Exceptions](https://clojure.org/reference/exceptions).