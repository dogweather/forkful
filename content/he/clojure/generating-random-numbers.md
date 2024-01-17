---
title:                "יצירת מספרים אקראיים"
html_title:           "Clojure: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# מה ולמה?
כתיבת קוד מפוצל זהו פעולה נפוצה בעולם התכנות, ואחת המטרות האפשריות שלו היא יצירת מספרים אקראיים. כתיבת קוד שמייצר מספרים אקראיים חשובה לשימושים שונים כמו ייצוג נתונים, מבחנים וייצוג חסריות.

# איך לעשות זאת?
```Clojure
;; הגדרת פונקציה שמגרילה מספר בין 0 ל-100:
(defn generate-random
  "מחזיר מספר אקראי משורה 0 למספר המקסימלי"
  [max]
  (rand-int max))

;; הדפסת מספר אקראי בין 0 ל-100:
(print (generate-random 100))

```
הפתרון שלנו משתמש בפונקציה פנימית שנקראת `rand-int` המייצרת מספר אקראי בין 0 למספר המקסימלי שנעביר לה כארגומנט.

### פלט דוגמא:
```Clojure
57
```

# חקירה מהימנת
כתיבת קוד מפוצל והגרלת מספרים אקראיים הם יכולות קריטיות בתחום התכנות. מאז כתיבת קוד מפוצל מוקדם מאוד, בשנת 1951, ספרי הלכלוך עוסקים ביצירת מספרים אקראיים טובים הוא בטלה. כיום, ישנן גם אלגוריתמים אחרים שניתן להשתמש בהם לייצור מספרים אקראיים בצורה טובה יותר.

## דוגמאות לפלט:
https://www.youtube.com/watch?v=duKKWOSIv8k

## Profile-literal


Czy random liczby to w sumie różnica

Aby ניתן היה להפעיל את הקוד שלנו, השתמשנו בהגדרה של פונקציה עם מספר המקסימלי שונה מאלפי. זהו נושא קריטי להבנה נכונה של כתיבת קוד מפוצל ותוכניות אחרות שמשתמשות במספרים אקראיים כמקובלות.

# ראה גם
1. [The Clojure Rand Library] (https://github.com/clojure/math.numeric-tower)
2. [הגרלת מספרים אקראיים בקונטרה] (https://en.wikipedia.org/wiki/Random\_number\_generation)