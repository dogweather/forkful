---
title:                "Clojure: הדפסת פלט ניתוח שגיאות."
simple_title:         "הדפסת פלט ניתוח שגיאות."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

תלחצו על לתיאור מאת פלומי שון שבו הוא מתאר את השימוש בהדפסת פלת מעקב כתוצר מקונסולת לוגים

## איך לעשות

הדפסת פלט מעקב היא כלי חשוב בתהליך הפיתוח של תוכניות ביסאו, המאפשרת למתכנתים לבדוק ולאתר בעיות ולנתח את הקוד שלהם. כאן יש להוסיף כמה דוגמאות מומלצות ופלט לדוגמאות האלה באמצעות בלוקי קוד Clojure (```Clojure ... ```).

לדוגמה:

```Clojure
(def speed 100)

(println "Current speed:" speed)

;; אופן פלט:
;; Current speed: 100
```

```Clojure
(def names ["Eli" "Yael" "David"])

(doseq [name names]
  (println "Hello " name))

;; אופן פלט:
;; Hello Eli
;; Hello Yael
;; Hello David
```

כמו כן, ניתן להשתמש בפקודת ```(pprint)``` על מנת להדפיס ערכים מורכבים כמו מפתחות מילון או רשימות.

```Clojure
(def person {:name "Moshe" :age 30})

(pprint person)

;; אוטפוט:
;; {:name "Moshe"
;;  :age 30}
```

## הכנסה עמוקה

הדפסת פלט מעקב יכולה להיות מאוד מועילה לנתח תוכניות ולזהות כישלונות. ניתן להשתמש בפונקציות כמו ```(pr)``` (להדפיס ערכים מורכבים בצורה זורמת) או ```(spy)``` (להדפיס את ערכי הפרמטרים שהועברו לפונקציה) כדי להבין את מהלך התוכנית.

כמו כן, ניתן להשתמש בכלי עזר כמו [clojure.tools.logging](https://github.com/clojure/tools.logging) כדי לנהל את היציאות ולהציג רמות שונות של מידע בהתאם לצורך.

## ראו גם

למידע נוסף על הדפסת פלט מעקב בקוד Clojure, ניתן להשתמש במדריכים הבאים:

- [מדריך מתקדם להדפסת פלט מעקב](https://clojure.org/guides/advanced_trace_printing)
- [כמה טיפים יע