---
title:                "Clojure: כתיבה לתקליט התקנה"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

חשוב לידע שכתיבה לשגיאות סטנדרטיות בקוד Clojure יכולה לעזור לך לעקוב אחר תהליכי הרצה ולזהות בעיות מהיר יותר. בנוסף, כתיבה לשגיאות סטנדרטיות יעזור לשפר את איכות הקוד ולשמור על נקיון וארגון.

## איך לעשות זאת

הנה דוגמא של קוד Clojure שמדגים איך לכתוב לקונסולה שגיאה בעזרת הפונקציה `println`:

```Clojure
(println :error "תוכן הטקסט")
```

פלט הקוד יהיה:

```
:error תוכן הטקסט
```

כדי לכתוב לאזור השגיאה המקורי של התוכנית, ניתן להשתמש בפונקציה `eprint`:

```Clojure
(eprint "קוד שגיאה:" :error "12345")
```

פלט הקוד יהיה:

```
קוד שגיאה: :error 12345
```

## תהליך מקיף

השתמש בפונקציות כתיבה לשגיאות סטנדרטיות רק כאשר זה מתאים למקרה הספציפי. ניתן גם להשתמש בעזרת פונקציות פיתוח נוספות כמו `assert` ו-`ex-info` לשליחת מידע נוסף על השגיאה. כלכלת ההחלטה תלויה במטרות התכנית ועל איזה רמת פרטיוניות אתה רוצה לאפשר.

## ראה גם

- חומר לימוד על כתיבה לשגיאות סטנדרטיות ב-Clojure: https://www.clojure.org/guides/faq#error_handling
- דוגמאות לשימוש בפונקציות לכתיבה לשגיאות סטנדרטיות: https://clojureverse.org/t/logging-and-exception-management-on-a-recent-clojure-koans-marathon/2179
- שימוש פרקטי של כתיבה לשגיאות סטנדרטיות בפרוייקט ממקור פתוח: https://github.com/Untangled-web/untangled-web/blob/aece1b57a8421b3a805ded01c1e52b030596c595/src/clj/untangled/web.clj#L375