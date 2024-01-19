---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?: 
מציאת אורך המחרוזת היא פשוטה - זה הספירה של כמה תווים יש במחרוזת. מתכנתים מציאים את אורך מחרוזת כדי לבדוק את תקינותה ולאפשר תנאים שונים בביצוע הקוד.
 
## איך לעשות:

חלקקים אחדים של קוד אמיתי למציאת אורך המחרוזת ב-Clojure:
 
```Clojure
(defn string-length
  [s]
  (count s))

(println (string-length "Hello, World!"))
```
יחזיר: `13` 
 
```Clojure
(defn empty-string?
  [s]
  (= 0 (count s)))

(println (empty-string? ""))
```
יחזיר: `true` 

## ירידה עמוקה:
ראשית, Clojure הוא שפת תכנות פונקציונלית מבית משפחת Lisp, שמתייחסת למחרוזות כאל רשימת תווים. לכן, Count היא בעצם פונקציה שמונה את מספר התווים.

חלופות לפונקציה של `count` הן `size` או `length`, אבל ב-Clojure הן לא קיימות.
 
לאחר מכן, אם אתה רוצה להכניס את מספר התווים של מחרוזת לונקציה אחרת או לתנאי, אתה יכול להשוות אותו ישירות או לבצע את פונקציית המחרוזת.

##ראה גם:
נסה את הקישורים הבאים על מנת לבחון בפרטים נוספים את הנושא של ניתוח מחרוזות בClojure:
 
1. Clojure מדריך מחרוזות: https://clojuredocs.org/clojure.core/string
2. התחלה מחרוזות ב-Clojure: https://www.learn-clojure.com/clojure-strings
3. ספרות על Clojure: https://clojure.org/guides/readme