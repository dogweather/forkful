---
title:                "Clojure: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

חישוב תאריך בעתיד או בעבר הוא כלי חשוב בתכנות ב-Clojure כאשר יש לנו צורך לעבוד עם תאריכים בצורה מדויקה.

## איך לעשות את זה

הנה כמה דוגמאות לחישוב תאריך בעתיד או בעבר בשפת Clojure:

```
Clojure (clj-lib.time/date-fn (clj-lib.time/today) 7 :days)
```
מפקסים את התאריך הנוכחי ומוסיפים 7 ימים אליו. הפלט יהיה תאריך אחד שבוע מהיום.

```
Clojure (clj-lib.time/date-fn (clj-lib.time/today) 2 :months)
```
מפקסים את התאריך הנוכחי ומוסיפים 2 חודשים אליו. הפלט יהיה תאריך אחד חודשים מהיום.

```
Clojure (clj-lib.time/date-fn (clj-lib.time/today) -1 :year)
```
מפקסים את התאריך הנוכחי ומחסירים שנה אחד ממנו. הפלט יהיה תאריך חצי שנה לפני היום.

כמו כן, ניתן להשתמש גם בפונקציית `(clj-lib.time/date)` כדי לקבל תאריך ספציפי בעבור יום, חודש ושנה מסוימים.

## חפירה עמוקה

כאשר אנחנו מפענחים תאריך בשפת Clojure, התאריך מיוצג כמספר שלם של ימים מה-1 לינואר 1970, שיקולו האפס. ניתן להשתמש בפונקציית `(clj-lib.time/date-fn)` כדי להוסיף או לחלק ימים, חודשים או שנים לתאריך ספציפי. כמו כן, ניתן להשתמש בפונקציית `(clj-lib.time/date)` כדי ליצור תאריך חדש לפי יום, חודש ושנה ספציפיים.

## ראה גם

- [clj-lib.time ארכיון תיעוד](https://github.com/clj-lib/time)
- [עשר מעטפות תאריך הכי פופולריות לשפות תכנות](https://www.sitepoint.com/top-10-date-time-manipulation-tools-and-libraries/)