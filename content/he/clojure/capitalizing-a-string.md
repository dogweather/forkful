---
title:    "Clojure: כיתוב מילות התחלה באופן גדול"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה?

למה ליצור תכנית ב-Clojure שמחלקת מחרוזת לאותיות גדולות? 

## כיצד לעשות זאת

```Clojure
;; הכנסת מחרוזת כארגומנט לפונקציה
(defn capitalizer [string] 
  ;; פיצול המחרוזת לרשימת תווים
  (loop [chars (seq string), ;; השינויים יעשו על כל יחידת התו במחרוזת
         result ""] ;; ייצור תוצאת החלוקה
    (if (seq chars)
      (recur (rest chars) ;; העברת כל אחת מהתווים לפונקציה הבאה
             (conj result (char (if (>= (a-65) ;; המאתים של כל אותיות האנגלית הקטנות הם 97-122. על כן, רק תווים המתאימים יעבירו
                                      (first chars)
                                      ;; השינוי לאות גדולה מתאים למשמעות של המשתנה "chars"
                                      (- (first chars) 32))))))) 
      result))

;; קריאת הפונקציה
(capitalizer "hello world") 
;; פלט: "HELLO WORLD"
```

## העיון העמוק

ישנם כמה שיטות שונות להגדיל מחרוזות ב-Clojure. בתוך הפונקציה שרץ 'recur', מבצעת תהליך מסוים שנהיה רקורסיבי.

כמה מאפיינים מעניינים נוספים של הקוד הזה:
- כל תו מועבר לפונקציה המתאימה: 2 בלולאה.
- לא כל הפונקציות מכילות בהכרזת שינויים בכיתוב.
- אמור להישמע לכם כך.
- זיכרון נלקח מASCII ב-Seque experiments https://seque.blogspot.com/2009/12/ascii-codes-in-duct.html#L109-L118.

## ראו גם

- מידע נוסף על קוד:  https://clojure.org/api-index
- פונקציות נוספות לעבודה עם מחרוזות: https://clojuredocs.org/clojure.string/split