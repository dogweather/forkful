---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:13:43.734769-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת התאריך הנוכחי בתכנות זה סתם דרך לשאול את המחשב: "איזה יום זה היום?" תכניתנים עושים את זה כדי לתייג אירועים, לחשב פרקי זמן, או לתזמן פעולות.

## איך לעשות:
Clojure מספק ספרייה שנקראת `java.time` דרך Java Interop לעבוד עם תאריכים. ככה תעשו את זה:

```clojure
;; ייבוא של הספרייה
(require '[java.time :as time])

;; קבלת התאריך הנוכחי
(defn get-current-date []
  (str (time/LocalDate/now)))

;; דוגמה לשימוש
(get-current-date)  ; => "2023-04-07"
```

## ריקוד עמוק:
בעבר, בג'אווה ולכן גם ב-Clojure, היה נהוג להשתמש בספריית `java.util.Date`. אבל, זו הייתה בעייתית ולא נוחה. העולם התקדם, והחליף ל`java.time` בג'אווה 8, שהיא בהרבה יותר תקינה ואינטואיטיבית. ב-Clojure, גישה נפוצה היא להשתמש בJava Interop כדי לממש את אותן פקודות, אבל בצורה "קלוג'רית". אפשרויות אלטרנטיביות כוללות ספריות כמו `clj-time`, אבל כיום רבים יעדיפו את הממשק המובנה של `java.time`. 

## ראה גם:
- [Clojure Documentation](https://clojure.org/)
- [java.time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)