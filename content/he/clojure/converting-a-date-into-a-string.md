---
title:                "המרת תאריך למחרוזת"
html_title:           "Clojure: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
תחשוב על המצב הבא: אתה מפתח אפליקציה בקלוז'הר ואתה צריך להציג תאריך בפורמט של מחרוזת. כיצד תעשה זאת בצורה יעילה וכיצד ניתן להבין יותר על התאריך עצמו בתהליך המרתו למחרוזת? זאת הסיבה שבגללה נעסוק בתהליך המרת תאריך למחרוזת בקלוז'ר.

## איך לעשות זאת
עבור קלוז'הר למחרוזת ישנם כמה אפשרויות:

```Clojure
(def my-date (java.util.Date.)) ;יצירת תאריך נוכחי
(str my-date) ;המרת התאריך למחרוזת בפורמט של ברירת מחדל
; output: "Sun Dec 05 16:15:54 IST 2021"

(import java.time.format.DateTimeFormatter)

(def formatter (DateTimeFormatter/ofPattern "dd/MMM/yyyy")) ;יצירת פורמט תאריך מותאם אישית
(formatter.format (java.time.LocalDateTime/now)) ;המרת התאריך הנוכחי למחרוזת בפורמט של "dd/MMM/yyyy"
; output: "05/Dec/2021"
```

שים לב שכאשר אנחנו ממירים תאריך למחרוזת, אנחנו יכולים להשתמש בפורמטים שונים כדי לייצג אותו באופן שונה. כמו כן, גם ניתן לבחור פורמט של תאריך מותאם אישית, כך שנוכל לייחס לתאריך בצורה מדויקת יותר.

## העומק של התהליך
כאשר אנו ממירים תאריך למחרוזת, אנו משתמשים בפונקציית `str` המשמשת להמרת אובייקטים שונים למחרוזת. כמו כן, ניתן להשתמש בפונקציית `format` כדי לייצג את התאריך בפורמט מותאם אישית. בקלוז'הר ישנם גם מספר ספריות נוספות שיכולות לעזור בתהליך המרת תאריך למחרוזת, כגון `clj-time` ו-`java-time`.

## ראה גם
- [פונקציית `str` בקלוז'הר](https://clojuredocs.org/clojure.core/str)
- [פונ