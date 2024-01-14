---
title:    "Clojure: קבלת התאריך הנוכחי"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מדוע
יצירת קשר עם התאריך הנוכחי הוא חלק חשוב מתחום התכנות בשפת קלוז'רה. זה מאפשר לך ליצור יישומים תפוסים המשתמשים בתאריך הנוכחי, כגון ייצוג התאריך בפורמט מבוקש או חישוב תאריכים עבור נוסחאות מחשבון כמהותטבח.
 
## איך
באמצעות הפונקציה ```Clojure (java.util.Date.)``` ניתן לבקש את התאריך הנוכחי בשפת קלוז'רה. 

באמצעות הפקודות הבאות, אנו יוצרים את התאריך הנוכחי ומציגים אותו בפורמט מבוקש:
```Clojure
(def current-date (java.util.Date.))

(format "%tc" current-date) ;=> יום שני 9 במאי 2021 11:54:42
(format "%te/%tm/%ty" current-date) ;=> 09/05/21
(format "%tB %tY" current-date) ;=> מאי 2021
```

כדי לחשב תאריכים עבור נוסחאות מחשבון כמהות ופעולות אחרות, ניתן להשתמש בספריית ```Clojure clojure.java-time``` שמאפשרת פעולות יעילות על תאריכים:
```Clojure
(require '[java-time :as jt]) ;=> nil

(jt/now) ;=> #object[java.time.LocalDateTime 0x7d546d6a "2021-05-09T11:54:42.557889700"]
(jt/plus (jt/now) (jt/period "P3M")) ;=> #object[java.time.LocalDateTime 0x1c1b1d8e "2021-08-09 11:54:42.557889700"]

```

## עיון מעמיק
הוספת תאריך ליישומים שלך עשויה להיות מעניין שאתה צריך לעבוד עם תאריך מסוים בעל הפוקוף הרצוי או להשוות תאריך הנוכחי לתאריך נתון. 
למשל, כדי להשוות בין תאריכים, ניתן להשתמש בפונקציות ```Clojure before?``` ו- ```after?```:
```Clojure
(before? (jt/now) (jt/plus (jt/now) (jt/period "P3M"))) ;=> true
(after? (jt/now) (jt/plus (jt/now) (jt/period "P3M"))) ;=> false
```

כמו כן, אם תאריך מסוים נדרש כארגומנט לפונקציה, ניתן להשתמש בפונ