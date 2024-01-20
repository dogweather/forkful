---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה & למה?
קבלת התאריך הנוכחי הוא הפרוצדורה של שליפת נתוני תאריך ושעה "חיים" מהמערכת. תכנתים משתמשים בזה לצורך ניתוחים, רישום ומעקב אירועים, וארגונים של נתונים.

## איך לעשות:
קוד ב-Clojure שמקבל את התאריך ושעה הנוכחיים הינו:
```Clojure 
    (require '[java-time :as jt])
    (jt/local-date)
```
הקוד מדפיס תאריך מופשט בצורת `YYYY-MM-DD` , על פי שעת המשתמש.

## צלילה עמוקה:
מימי התכנות הראשונים, התאריך הנוכחי תמיד היה משאב חיוני.
### אלטרנטיבות:
אפשר גם לחלץ את הזמן, בנוסף לתאריך, עם פונקציה `(jt/local-date-time)` :
```Clojure
    (require '[java-time :as jt])
    (jt/local-date-time)
```
### פרטים על היישום:
מודול `java-time` ב-Clojure מספק גישה יעילה ל-Kotlin's java.time API. זה מאפשר לנו ליהנות מכלל התכונות של הממשק.

## ראו גם:
* פוסט נהדר של סקוט וודוורד העוסק ב- `java.time` : https://clojure.java-time.org/
* הוראות להתקנת סביבת Clojure: https://clojure.org/guides/getting_started
* `java.time` API ב- Java 8: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html